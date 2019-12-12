{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Language.SPar
  ( SPar (..)
  , CPar
  , printCPar
  , PID
  , DType (..)
  , participants
  , partsL
  , projTy
  , ifst
  , isnd
  , ATy (..)
  , ATyFn (..)
  , TyFun (..)
  , commStruct
  , pprSType
  -- , getTyRep
  , send
  , recv
  , trecv
  , run
  , mCse
  , branch
  , select
  , isInl
  , isInr
  , simpl
  , codeGen
  , declareParFun
  , earlySend
  ) where

import Type.Reflection
import Control.Monad.Extra ( (<$!>) )
import Control.Monad.State.Strict ( State, get, put, evalState )
import Data.List (nub)
import qualified Data.Set as Set
import           Data.Set  ( Set )

import Control.Monad.CGen
import Data.C
import Language.Alg


data SPar v t a where
  MSnd :: CVal a
       => !(v a)
       -> !PID
       -> !(SPar v t b)
       -> SPar v t b

  MRcv :: CVal a
       => !PID
       -> !(TypeRep a)
       -> !(v a -> SPar v t b)
       -> SPar v t b

  MRun :: (CVal a, CVal b)
       => !(t a b)
       -> !(v a)
       -> !(v b -> SPar v t c)
       -> SPar v t c

  MCse :: (CVal a, CVal b, CVal c)
       => !(v (Either a b))
       -> !(v a -> SPar v t (v c))
       -> !(v b -> SPar v t (v c))
       -> !(v c -> SPar v t d)
       -> SPar v t d

  MIf :: CVal a
       => !(v Bool)
       -> !(SPar v t (v a))
       -> !(SPar v t (v a))
       -> !(v a -> SPar v t b)
       -> SPar v t b

  MRet :: !a
       -> SPar v t a

isVar :: Alg t -> Integer -> Bool
isVar (BVar i) j | i Prelude.== j = True
isVar _ _ = False

isCase :: CPar a -> Bool
isCase (MCse l kl kr k)
  | isVar l 0 = True
  | otherwise = isCase (kl (BVar 1))
                || isCase (kr (BVar 1))
                || isCase (k (BVar 1))
isCase (MSnd _ _ k) = isCase k
isCase (MRcv _ _ k) = isCase (k (BVar 1))
isCase (MRun _ _ k) = isCase (k (BVar 1))
isCase _ = False


isInl :: CPar a -> Bool
isInl = pRet iil
  where
    iil (Inl _) = True
    iil _ = False

isInr :: CPar a -> Bool
isInr = pRet iir
  where
    iir (Inr _) = True
    iir _ = False

pRet :: (Alg a -> Bool) -> CPar a -> Bool
pRet p (MRet x) = p x
pRet p (MCse _ _ _ k) = pRet p (k (BVar 0))
pRet p (MRun _ _ k) = pRet p (k (BVar 0))
pRet p (MRcv _ _ k) = pRet p (k (BVar 0))
pRet p (MSnd _ _ k) = pRet p k
pRet p (MIf _ _ _ k) = pRet p (k (BVar 0))

printCPar :: Integer -> CPar a -> String
printCPar l (MSnd v p k)
  = "send " ++ printAlg l v ++ " " ++ show p ++ " ;\n" ++ printCPar l k
printCPar l (MRcv p _ k)
  = "recv " ++ show p ++ " ; /?" ++ show l ++ ". \n "
  ++ printCPar (l+1) (k (BVar l))
printCPar l (MRun f x k)
  = "run " ++ printAlg l (unFun f) ++ " (" ++ printAlg l x
  ++ ") ; /?" ++ show l ++ ". \n" ++ printCPar (l+1) (k (BVar l))
printCPar l (MCse v x y k)
  = "case (" ++ printAlg l v ++ ") \n " ++
    "(/?" ++ show l ++ ". " ++ printCPar (l+1) (x (BVar l)) ++ ") \n" ++
    "(/?" ++ show l ++ ". " ++ printCPar (l+1) (y (BVar l)) ++ ") ; /?"
    ++ show l ++ ". \n " ++ printCPar (l+1) (k (BVar l))
printCPar l (MIf v x y k)
  = "if (" ++ printAlg l v ++ ") \n " ++
    "(/?" ++ show l ++ ". " ++ printCPar l x ++ ") \n" ++
    "(/?" ++ show l ++ ". " ++ printCPar l y ++ ") ; /?"
    ++ show l ++ ". \n " ++ printCPar (l+1) (k (BVar l))
printCPar l (MRet x) = "end " ++ printAlg l x

newtype TyFun a b = TyFun { unTyFun :: TypeRep (a -> b) }

getType :: CVal a => v a -> TypeRep a
getType _ = typeRep

getTyFun :: (CVal a, CVal b) => t a b -> TyFun a b
getTyFun _ = TyFun typeRep

type SType a = SPar TypeRep TyFun (TypeRep a)

commStruct :: (CVal a, CAp t v) => SPar v t (v a) -> SType a
commStruct (MSnd v p k) = MSnd (getType v) p (commStruct k)
commStruct (MRcv p t k) = MRcv p t (\_ -> commStruct (k var))
commStruct (MRun f v k) =
  MRun (getTyFun f) (getType v) (\_ -> commStruct (k var))
commStruct (MCse v kl kr k) =
  MCse (getType v)
    (\_ -> commStruct (kl var))
    (\_ -> commStruct (kr var))
    (\_ -> commStruct (k var))
commStruct (MIf v kl kr k) =
  MIf (getType v)
    (commStruct kl)
    (commStruct kr)
    (\_ -> commStruct (k var))
commStruct (MRet _) = MRet typeRep

pprSType :: SType a -> String
pprSType (MSnd v p k) =
  show p ++ " ! <" ++ show v ++ ">; " ++
  pprSType k
pprSType (MRcv p t k) =
  show p ++ " ? (" ++ show t ++ "); " ++
  pprSType (k typeRep)
pprSType (MRun f v k) =
  "{ " ++ show (unTyFun f) ++ " } " ++ show v ++ "; " ++
  pprSType (k typeRep)
pprSType (MCse v kl kr k) =
  "case (" ++ show v ++ ") (" ++
  pprSType (kl typeRep) ++ " | " ++
  pprSType (kr typeRep) ++ "); " ++
  pprSType (k typeRep)
pprSType (MIf _v kl kr k) =
  "if (" ++
  pprSType kl ++ " | " ++
  pprSType kr ++ "); " ++
  pprSType (k typeRep)
pprSType (MRet v) =
  "end (" ++ show v ++ ")"


type CPar a = SPar Alg (:->) (Alg a)

send :: CVal a => Alg a -> PID -> CPar ()
send v p = MSnd v p (MRet (Lit ()))

bcast :: CVal a => Alg a -> [PID] -> CPar ()
bcast _ []     = MRet (Lit ())
bcast v (p:ps) = MSnd v p $ bcast v ps


data DType a where
  DPair  :: (CVal a, CVal b)
         => !(DType a)
         -> !(DType b)
         -> DType (a, b)

  DTagL :: (CVal a, CVal b)
        => !(DType a)
        -> CTy b
        -> DType (Either a b)

  DTagR :: (CVal a, CVal b)
        => CTy a
        -> !(DType b)
        -> DType (Either a b)

  -- DVec :: CVal a => [DType a] -> DType [a]

  DVal :: CVal a => PID -> CTy a -> DType a
  DAlt :: CVal a => PID -> !(DType a) -> !(DType a) -> DType a

--getTyRep :: DType a -> TypeRep a
--getTyRep (DPair _ _) = typeRep
--getTyRep (DTagL _ _) = typeRep
--getTyRep (DTagR _ _) = typeRep
--getTyRep (DVal _ _) = typeRep
--getTyRep (DAlt _ _ _) = typeRep

partsL :: DType a -> [PID]
partsL d = force ppd `seq` ppd
  where
    ppd = nub $! pp d []
    force [] = ()
    force (x:xs) = x `seq` force xs
    pp :: DType b -> [PID] -> [PID]
    pp (DAlt p l r) !ps = pp l (pp r (p:ps))
    pp (DVal p _) !ps = p : ps
    pp (DTagL p _) !ps = pp p ps
    pp (DTagR _ p) !ps = pp p ps
    pp (DPair l r) !ps = pp l (pp r ps)

participants :: DType a -> Set PID
participants (DAlt p l r) = pp `seq` pp
  where
    !pl = participants l
    !pr = participants r
    !pp = Set.insert p $! Set.union pl pr
-- participants (DVec ps) = Set.unions $ map participants ps
participants (DVal p _) = Set.singleton p
participants (DTagL p _) = participants p
participants (DTagR _ p) = participants p
participants (DPair l r) = p `seq` p
  where
    !p = pl `Set.union` pr
    !pl = participants l
    !pr = participants r


ifst :: (CVal a, CVal b) => DType (a, b) -> DType a
ifst (DVal p (CPair l _)) = DVal p l
ifst (DPair l _) = l
ifst (DAlt p l r) = DAlt p (ifst l) (ifst r)

isnd :: (CVal a, CVal b) => DType (a, b) -> DType b
isnd (DVal p (CPair _ r)) = DVal p r
isnd (DPair _ r) = r
isnd (DAlt p l r) = DAlt p (isnd l) (isnd r)

data ATy where
  ATy :: CVal a => !(CTy a) -> ATy

instance Eq ATy where
  ATy l == ATy r = case eqTypeRep (getCTyR l) (getCTyR r) of
                     Just _ -> True
                     Nothing -> False

data ATyFn where
  ATyFn :: (CVal a, CVal b) => TypeRep a -> TypeRep b -> ATyFn

projTy :: DType a -> PID -> ATy
projTy i p
  | not (p `Set.member` pri) = ATy CUnit
  where
    !pri = prr `seq` prr
    !prr = participants i
projTy (DPair l r) p = case (projTy l p, projTy r p) of
                         (ATy ll, ATy rr) -> ATy $! CPair ll rr
projTy (DTagL i _) p = projTy i p
projTy (DTagR _ i) p = projTy i p
projTy (DVal pt t) p
  | p Prelude.== pt = ATy t
  | otherwise = ATy CUnit
projTy i@(DAlt _ l r) p
  | p `Set.member` participants i = case (tl, tr) of
                                      (ATy tll, ATy trr) -> ATy $ CEither tll trr
  | otherwise = if tl Prelude.== tr then tl else error "Error: ill-formed interface"
  where
    tl = projTy l p
    tr = projTy r p

recv :: forall a. CVal a => PID -> CPar a
recv p = MRcv p typeRep MRet

trecv :: forall a t. CVal a => t a -> PID -> CPar a
trecv _ p = MRcv p typeRep MRet

run :: (CVal a, CVal b) => a :-> b -> Alg a -> CPar b
run f v = MRun f v MRet

mCse :: (CVal a, CVal b, CVal c)
     => Alg (Either a b)
     -> (Alg a -> CPar c)
     -> (Alg b -> CPar c)
     -> CPar c
mCse (Inl v) f _ = f v
mCse (Inr v) _ g = g v
mCse (Case v l r) f g = mCse v (\x -> mCse (ap l x) f g) (\x -> mCse (ap r x) f g)
mCse mv f g = MCse mv f g (\x -> MRet x)

branch :: CVal a => PID -> CPar a -> CPar a -> CPar a
branch p l r = recv p >>= \(x :: Alg (Either () ())) ->
                            mCse x (\_ -> l) (\_ -> r)

select :: (CVal a, CVal b, CVal c)
       => Alg (Either a b)
       -> [PID]
       -> (Alg a -> CPar c)
       -> (Alg b -> CPar c)
       -> CPar c
select v ps l r = mCse v cl cr
  where
    cl t = bcast (Inl (Lit ()) :: Alg (Either () ())) ps >> l t
    cr t = bcast (Inr (Lit ()) :: Alg (Either () ())) ps >> r t

instance CAp t v => Functor (SPar v t) where
  fmap f (MSnd x p k) = MSnd x p (fmap f k)
  fmap f (MRcv p t k) = MRcv p t $ fmap (fmap f) k
  fmap f (MRun t a k) = MRun t a $ fmap (fmap f) k
  fmap f (MCse e kl kr k) = MCse e kl kr (fmap (fmap f) k)
  fmap f (MIf e kl kr k) = MIf e kl kr (fmap (fmap f) k)
  fmap f (MRet v) = MRet $ f v

instance CAp t v => Applicative (SPar v t) where
  pure = MRet
  af <*> av = join (fmap ((`fmap` af) . flip ($)) av)

join :: SPar v t (SPar v t a) -> SPar v t a
join (MSnd e p k) = MSnd e p $! join k
join (MRcv p t k) = MRcv p t $! fmap join k
join (MRun t a k) = MRun t a $! fmap join k
join (MCse e kl kr k) = MCse e kl kr $! fmap join k
join (MIf e kl kr k) = MIf e kl kr $! fmap join k
join (MRet f) = f

instance CAp t v => Monad (SPar v t) where
  MSnd e p k     >>= kf = MSnd e p $! k >>= kf
  MRcv p t k     >>= kf = MRcv p t $! \x -> k x >>= kf
  MRun t a k     >>= kf = MRun t a $! \x -> k x >>= kf
  MCse e kl kr k >>= kf = MCse e kl kr $! \x -> k x >>= kf
  MIf e kl kr k  >>= kf = MIf e kl kr $! \x -> k x >>= kf
  MRet x         >>= kf = kf x

type FVM a = State Integer a

next :: FVM Integer
next = get >>= \s -> put (s+1) >> pure s

takePrefix :: CVal a
           => Set Integer
           -> Set PID
           -> (CPar a -> CPar a)
           -> (CPar a -> CPar a)
           -> CPar a
           -> FVM (CPar a -> CPar a, CPar a)
takePrefix ll ps pre post (MSnd e p' kp')
  | ll `Set.disjoint` fbvse && p' `Set.notMember` ps
  = takePrefix ll ps (\x -> pre (MSnd e p' x)) post kp'
  | otherwise
  = takePrefix ll (Set.insert p' ps) pre (\x -> post (MSnd e p' x)) kp'
  where
    fbvse = fbvs e
takePrefix ll ps pre post (MRun f a kp')
  | ll `Set.disjoint` fbvse = do
      nl <- next
      takePrefix ll ps (\x -> pre (MRun f a $! close nl x)) post $ kp' $ BVar nl
  | otherwise = do
      nl <- next
      let nll = Set.insert nl ll
      takePrefix nll ps pre (\x -> post (MRun f a $! close nl x)) $ kp' $ BVar nl
  where
    fbvse = fbvs a `Set.union` fbvs (unFun f)
takePrefix _ _ pre post k =
  pure (pre, post k)
--takePrefix ll pre post e@(MRun f a ks)
--  | ll `Set.disjoint` fbvsa = do
--      nl <- next
--      takePrefix ll (\x -> pre (MRun f a $ close nl x)) (ks $ BVar nl)
--  | otherwise
--  = pure $ (pre, e)
--  where
--    fbvsa = fbvs a
--takePrefix _ pre post x
--  = pure (pre, x)

earlySend :: CVal a => CPar a -> FVM (CPar a)
earlySend (MSnd e p k) = MSnd e p <$!> earlySend k
earlySend (MRcv p t k) = do
  !l <- next
  !kl <- earlySend (k $ BVar l)
  !(pre, post) <- takePrefix (Set.singleton l) Set.empty id id kl
  pure <$!> pre $! MRcv p t $! close l $! post
earlySend (MRun t a k) = do
  !l <- next
  !kl <- earlySend (k $ BVar l)
  !(pre, post) <- takePrefix (Set.singleton l) Set.empty id id kl
  pure <$!> pre $! MRun t a $! close l $! post
earlySend (MCse e kl kr k) = do
  x <- next
  MCse e
    <$!> (close x <$!> earlySend (kl $ BVar x))
    <*> (close x <$!> earlySend (kr $ BVar x))
    <*> (close x <$!> earlySend (k $ BVar x))
earlySend (MIf e kl kr k) = do
  x <- next
  MIf e
    <$!> earlySend kl
    <*> earlySend kr
    <*> (close x <$!> earlySend (k $ BVar x))
earlySend f@MRet{} = pure f

close :: (CVal a, CVal b) => Integer -> CPar a -> Alg b -> CPar a
close i (MRet e) = \x -> MRet (closeAlg i e x)
close i (MIf e kl kr k) = \x ->
  MIf (closeAlg i e x) (close i kl x) (close i kr x) (\y -> close i (k y) x)
close i (MCse e kl kr k) = \x ->
  MCse (closeAlg i e x) (\y -> close i (kl y) x)
  (\y -> close i (kr y) x) (\y -> close i (k y) x)
close i (MRun t a k) = \x ->
  MRun (closeFun i t x) (closeAlg i a x) (\y -> close i (k y) x)
close i (MRcv p a k) = \x ->
  MRcv p a (\y -> close i (k y) x)
close i (MSnd e p k) = \x ->
  MSnd (closeAlg i e x) p $ close i k x

simpl :: CVal a => CPar a -> CPar a
simpl (MSnd e p k) = MSnd e p $ simpl k
simpl (MRcv p t k) = MRcv p t $ \x -> simpl (k x)
simpl (MRun t a k) = MRun t a $ \x -> simpl (k x)
simpl (MCse e kl kr k)
  | isCase (k (BVar 0)) && isInl (kl (BVar 0)) && isInr (kr (BVar 0))
  = mCse e (\x -> simpl $ kl x >>= k) (\x -> simpl $ kr x >>= k)
  | otherwise =
    mCse e (\x -> simpl $ kl x) (\x -> simpl $ kr x) >>= \y -> simpl (k y)
simpl (MIf e kl kr k) = MIf e (simpl kl) (simpl kr) $ \x -> simpl (k x)
simpl m@MRet{} = m

declareParFun :: (CVal a, CVal b) => String -> PID -> (Alg a -> CPar b) -> CGen ASt ()
declareParFun pn p f
  | eraseTy (domTy f) Prelude.== ECUnit = do
      !ctys <- cTySpec cty
      !(cv, body) <- codeGen p (f $ CVal $ cVar cUnit)
      newFun (fn, ctys) []
        (body ++ [CBlockStmt $ CReturn (Just cv) undefNode])

  | otherwise = do
      !dv <- freshVar
      !ctys <- cTySpec cty
      !dtys <- cTySpec dty
      !(cv, body) <- codeGen p (f $ CVal $ cVar dv)
      newFun (fn, ctys) [(dv, dtys)]
        (body ++ [CBlockStmt $ CReturn (Just cv) undefNode])
  where
    !fn = internalIdent $ pn ++ "_part_" ++ show p
    !dty = domTy f
    !cty = codTy f

codeGen :: CVal a => PID -> CPar a -> CGen ASt (CExpr, [CBlockItem])
codeGen self = cgen . (`evalState` 0) . earlySend
  where
    cgen :: forall a. CVal a => CPar a -> CGen ASt (CExpr, [CBlockItem])
    cgen (MSnd e@(CVal iv)  p  k) = do
      !s1 <- csend self p (getTy e) $ iv
      !(rv, s2) <- cgen k
      pure (rv, s1 ++ s2)
    cgen (MSnd e  p  k) = do
      !(v1, s1) <- compileAlg e
      !s2 <- csend self p (getTy e) v1
      !(rv, s3) <- cgen k
      pure (rv, s1 ++ s2 ++ s3)
    cgen (MRcv p ty fk) = do
      !v1 <- freshVar
      !dv1 <- v1 <:: ty
      !s1 <- crecv self p (getTy ty) $ cVar v1
      !(rv, s2) <- cgen (fk $ CVal $ cVar v1)
      pure (rv, dv1 ++ s1 ++ s2)
    cgen (MRun f e fk) = do
      !(v, s1) <- compileAlg (ap f e)
      !(rv, s2) <- cgen (fk $ CVal v)
      pure (rv, s1 ++ s2)
    cgen (MCse e kl kr fk) = do
      !vk <- freshVar
      !dvk <- vk <:: domTy fk
      !(ve, se) <- compileAlg e
      !(vkl, sl) <- cgen (kl $ CVal $ v1l ve)
      !(vkr, sr) <- cgen (kr $ CVal $ v1r ve)
      !(rv, sk) <- cgen (fk $ CVal $ cVar vk)
      pure (rv, dvk ++ se ++
             cs ve (sl ++ cret (cVar vk) vkl) (sr ++ cret (cVar vk) vkr) : sk)
        where
          b = getTy e Prelude.== ECEither ECUnit ECUnit
          v1l v1 | b = cVar cUnit
                 | otherwise = cMember (cMember v1 valFld) inlFld
          v1r v1 | b = cVar cUnit
                 | otherwise = cMember (cMember v1 valFld) inrFld
          mMember ce f
            | b = ce
            | otherwise = cMember ce f
          cs v1 sl sr = CBlockStmt $ cCase (mMember v1 tagFld) sl sr
    cgen (MIf e kl kr fk) = do
      !vv <- freshVar
      !dv <- vv <:: domTy fk
      !(v1, cb) <- compileAlg e
      !(vx, cx) <- cgen kl
      !(vy, cy) <- cgen kr
      !(rv, ck) <- cgen (fk $ CVal $ cVar vv)
      pure (rv, dv ++ cb ++
        (CBlockStmt $ CIf v1 (CCompound [] (cx ++ cret (cVar vv) vx) undefNode)
                      (Just $ CCompound [] (cy ++ cret (cVar vv) vy) undefNode)
                      undefNode) : ck)
    cgen (MRet e) = compileAlg e

domTy :: CVal a => (Alg a -> CPar c) -> CTy a
domTy _ = getCTy

codTy :: CVal c => (Alg a -> CPar c) -> CTy c
codTy _ = getCTy
