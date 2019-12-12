{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Language.SPar.Skel.Internal
  ( Env
  , CEnv
  , ATy
  , printEnv
  , domEnv
  , domEnvL
  , emptyEnv
  , kleisliEnv
  , splitEnv
  , caseEnv
  , choiceEnv
  , singleton
  , extendEnv
  , msg
  , efst
  , esnd
  , einl
  , einr
  , declareEnv
  ) where

import qualified Prelude
import Prelude hiding ( fst, snd, id )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Type.Reflection hiding ( Fun )
import qualified Data.Set as Set
import Data.Set ( Set )
import Data.List ( foldl' )
import Control.Monad.CGen

import Language.Alg hiding ( cCase )
import Language.SPar

data APar v t where
  APar :: forall a b v t. (CVal a, CVal b)
       => !(v a -> SPar v t (v b)) -> APar v t

aPar :: forall a b v t tr. (CVal a, CVal b)
       => tr a -> (v a -> SPar v t (v b)) -> APar v t
aPar _ = APar

domAPar :: CVal a => (v a -> SPar v t (v b)) -> TypeRep a
domAPar _ = typeRep

codAPar :: CVal b => (v a -> SPar v t (v b)) -> TypeRep b
codAPar _ = typeRep

type CAPar = APar Alg (:->)

-- FIXME: Instance Pretty
printAPar :: Integer -> CAPar -> String
printAPar l (APar f) = "/?" ++ show l ++ ". " ++ printCPar (l+1) (f (BVar l))

kleisli :: PID -> CAPar -> CAPar -> CAPar
kleisli pid (APar l) (APar r)
  = case eqTypeRep (codAPar l) (domAPar r) of
      Just HRefl -> APar $ \v -> l v >>= r
      Nothing -> error $ "Type error in kleisli composition for participant "
                 ++ show pid ++ ":"
                 ++ printCPar 1 (l (BVar 0))
                 ++ "\n" ++ printCPar 1 (r (BVar 0))
                 ++ "\n" ++ show (codAPar l)
                 ++ "\n" ++ show (domAPar r)

caseAPar :: CAPar -> CAPar -> CAPar
caseAPar (APar l) (APar r)
  = case eqTypeRep (codAPar l) (codAPar r) of
      Just HRefl -> aPar (eitherTy (domAPar l) (domAPar r)) $ \v -> simpl $ mCse v l r
      Nothing -> error $ "Type error in case: "
                 ++ "\n" ++ show (codAPar l)
                 ++ "\n" ++ show (codAPar r)
                 ++ "\n" ++ printCPar 1 (l (BVar 0))
                 ++ "\n" ++ printCPar 1 (r (BVar 0))

-- split <output participants> <current participant> f g
split :: Set PID -> PID -> CAPar -> CAPar -> CAPar
split p1 p2 (APar (l :: Alg a1 -> CPar b)) (APar (r :: Alg a2 -> CPar c))
  = case eqTypeRep (domAPar l) (domAPar r) of
      Just HRefl ->
        if p2 `Set.member` p1
        then APar $ \v -> l v >>= \x -> r v >>= \y -> pure (apair x y)
        else APar $ \v -> l v >>= \_ -> r v >>= \_ -> pure (Lit ())
      Nothing -> error $ "Type error in split for participant "
        ++ show p2 ++ "\n" ++ show p1 ++ ":"
        ++ printCPar 1 (l $ BVar 0) ++ "\n"
        ++ printCPar 1 (r $ BVar 0) ++ "\n"
        ++ show (domAPar l)        ++ "\n"
        ++ show (domAPar r)        ++ "\n"

newtype Env v t = Env { unEnv :: Map PID (APar v t) }

type CEnv = Env Alg (:->)

declareEnv :: String -> CEnv -> CGen ASt ()
declareEnv fn = mapM_ compileRole . Map.toList . unEnv
  where
    compileRole :: (PID, CAPar) -> CGen ASt ()
    compileRole (r, APar c) = declareParFun fn r c


printEnv :: CEnv -> String
printEnv (Env e) = concatMap showproc $ Map.toList $ fmap (printAPar 0) e
  where
    showproc (p, s) = show p ++ " |--> " ++ s ++ "\n\n"

domEnv :: Env v t -> Set PID
domEnv (Env e) = Map.keysSet e

domEnvL :: Env v t -> [PID]
domEnvL (Env e) = Map.keys e

-- Invariant: Map.lookup pid unProc :: Maybe (lookType pid outR)
--lookup :: forall a v t. CVal a => TypeRep a -> Env v t -> PID -> APar v t
--lookup _ env p = case Map.lookup p (unEnv env) of
--                   Just x -> x
--                   Nothing -> APar @a (\x -> MRet x)

emptyEnv :: CAp t v => Env v t
emptyEnv = Env { unEnv = Map.empty }

--unionEnv :: (APar v t -> APar v t -> APar v t) -> Env v t -> Env v t -> Env v t
--unionEnv f e1 e2 = Env { unEnv = Map.unionWith f (unEnv e1) (unEnv e2) }

unionEnvK :: (PID -> APar v t -> APar v t -> APar v t)
          -> Env v t -> Env v t -> Env v t
unionEnvK f e1 e2 =
  case Map.unionWithKey f (unEnv e1) (unEnv e2) of
    m -> Env m

extendEnv :: [(PID, ATy)] -> CEnv -> CEnv
extendEnv ps e = Env $ foldl' extend (unEnv e) ps
  where
    extend m (k, t) = Map.alter (doExtend t) k m
    doExtend :: ATy -> Maybe CAPar -> Maybe CAPar
    doExtend (ATy t) Nothing = let !a = aPar t $ \v -> pure v in Just a
    doExtend _ a = a

agreeDom :: CEnv -> CEnv -> (CEnv, CEnv)
agreeDom e1 e2 = (Env m1, Env m2)
  where
    k1 = Map.keysSet $ unEnv e1
    k2 = Map.keysSet $ unEnv e2
    k12 = k1 Set.\\ k2
    k21 = k2 Set.\\ k1
    idF = APar @() $ \v -> pure v
    m1 = Map.union (unEnv e1) $ Map.fromSet (\_ -> idF) k21
    m2 = Map.union (unEnv e2) $ Map.fromSet (\_ -> idF) k12

cCase :: Set PID -> PID -> CAPar -> CAPar -> CAPar
cCase ps k l r
      | k `Set.member` ps = caseAPar l r
      | otherwise         = l

caseEnv :: Set PID -> CEnv -> CEnv -> CEnv
caseEnv ps e1 e2 = unionEnvK (cCase ps) e1' e2'
  where
    (e1', e2') = agreeDom e1 e2

kleisliEnv :: CEnv -> CEnv -> CEnv
kleisliEnv !p1 !p2 = unionEnvK kleisli p1 p2

splitEnv :: Set PID -> CEnv -> CEnv -> CEnv
splitEnv ps !envL !envR = unionEnvK (split ps) envL envR

data Dir = L | R

projAPar :: Dir -> CTy a -> CAPar
projAPar L t@(CPair _ _) = let !a = aPar t $ \v -> pure (afst v) in a
projAPar R t@(CPair _ _) = let !a = aPar t $ \v -> pure (asnd v) in a
projAPar d (CEither l r) =
  case (projAPar d l, projAPar d r) of
    (APar f1, APar f2) ->
      let !a = aPar (eitherTy (domAPar f1) (domAPar f2)) $ \v ->
            simpl $ mCse v (\y -> f1 y >>= \x -> pure (Inl x))
            (\y -> f2 y >>= \x -> pure (Inr x))
      in a
projAPar _ _ = error "Projection on ill-typed term"

efst :: DType (a, b) -> [PID] -> CEnv
efst !t !ps = eproj t L ps

esnd :: DType (a, b) -> [PID] -> CEnv
esnd !t !ps = eproj t R ps

eproj :: DType (a, b) -> Dir -> [PID] -> CEnv
eproj t dr ps =
  force r `seq` Env $! Map.fromList r
  where
    force [] = ()
    force ((x, y):xs) = x `seq` y `seq` force xs
    !r = map projP ps
    projP !p = case projTy t p of
                 ATy tp -> let !pap = projAPar dr tp in (p, pap)

tagl :: Alg (Either () ())
tagl = Inl $ Lit ()

tagr :: Alg (Either () ())
tagr = Inr $ Lit ()

--type Lbl = Alg (Either () ())

sendLbl :: (CVal a, CVal b)
        => t a -> t b -> [PID] -> Alg (Either a b) -> CPar (Either a b)
sendLbl _ _ ps x
  | null ps = pure x
  | otherwise = mCse x (\_ -> sendAll tagl ps) (\_ -> sendAll tagr ps) *> pure x

sendAll :: Alg (Either () ()) -> [PID] -> CPar ()
sendAll t = foldl' (\m p -> send t p *> m) (pure $ Lit ()) Prelude.. reverse

recvLbl :: CVal a => DType a -> PID -> PID -> (PID, CAPar)
recvLbl i pc p =
  case projTy i p of
    ATy t -> (p, APar $ recvChoice t pc)

recvChoice :: CVal a => t a -> PID -> Alg a -> CPar (Either a a)
recvChoice _ p x = do
  y <- recv @(Either () ()) p
  mCse y (\_ -> pure (Inl x)) (\_ -> pure (Inr x))

choiceEnv :: CVal a => PID -> DType a -> DType a -> Set PID -> CEnv
choiceEnv p il ir ps =
  case (projTy il p, projTy ir p) of
    (ATy ipl, ATy ipr) -> Env $ Map.fromList $
      (p, APar $ sendLbl ipl ipr pp) : map (recvLbl ii p) pp
  where
    pp = Set.toList $ ps Set.\\ Set.singleton p
    ii = DAlt p il ir

msg :: DType a -> PID -> CEnv
msg int@(DAlt pc li ri) p
  | p `Set.notMember` ps = kleisliEnv (choiceEnv pc li ri $ Set.singleton p) env
  | otherwise = env
  where
    !ps = participants int
    !env = caseEnv (Set.insert p ps) (msg li p) (msg ri p)
msg (DVal pt t) p
  | p Prelude.== pt = Env $! Map.singleton p $! aPar t $! \v -> pure v
  | otherwise = env
  where
    !env = Env $ Map.fromList [(pt, snd), (p, rcv)]
    !snd = aPar t $ \v -> send v p
    !rcv = APar @() $ \_ -> trecv t pt
msg (DTagL l r) p = kleisliEnv (msg l p) env
  where
    !env = Env $ Map.singleton p $! aPar l $! \v -> pure (tinl v r)
msg (DTagR l r) p = kleisliEnv (msg r p) env
  where
    !env = Env $ Map.singleton p $! aPar r $! \v -> pure (tinr l v)
msg i@(DPair l r) p = unionEnvK (split $ Set.singleton p) envL envR
  where
    !envL = kleisliEnv pl $! msg l p
    !envR = kleisliEnv pr $! msg r p
    !pl = eproj i L $! partsL i
    !pr = eproj i R $! partsL i

doInj :: Dir -> ATy -> ATy -> CAPar
doInj L (ATy tl) (ATy tr) = aPar tl $ \v -> pure (tinl v tr)
doInj R (ATy tl) (ATy tr) = aPar tr $ \v -> pure (tinr tl v)

cinj :: (CVal b, CVal a) => DType a -> DType b -> Dir -> Set PID -> CEnv
cinj il ir d ps = Env $ Map.fromList $ map pinj $ Set.toList ps
  where
    pinj p = (p, doInj d (projTy il p) (projTy ir p))

einl :: (CVal b, CVal a) => DType a -> DType b -> Set PID -> CEnv
einl il ir = cinj il ir L

einr :: (CVal b, CVal a) => DType a -> DType b -> Set PID -> CEnv
einr il ir = cinj il ir R

singleton :: (CVal a, CVal b) => t a -> PID -> (Alg a -> CPar b) -> CEnv
singleton _ p f = Env $ Map.singleton p (APar f)
