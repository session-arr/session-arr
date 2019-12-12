{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SPar.Skel
  ( SProc (..)
  , CProc
  , printSProc
  , PM
  , (:=>)(..)
  , liftAlg
  , gather
  , AnnStrat
  , ann
  , printASkel
  , compileAsLib
  , annotate
  , ($)
  , inferGt
  , (:~>)(..)
  , OGT(..)
  , annGt
  , printGt
  , generateGtFile
  , msgGt
  , gatherGt
  , gatherNewGt
  , liftGt
  , constGt
  , Semigroup (..)
  , Monoid (..)
  , module X
  ) where

import qualified Prelude
import Prelude hiding ( (.), fst, snd, id, const )

import Control.Monad.RWS.Strict hiding ( lift, ap, fix )
import qualified Data.Set as Set
import Data.List ( intersperse )
import Data.Text.Prettyprint.Doc ( pretty )

import Data.Ratio (numerator, denominator)

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

import Control.CCat as X
import Control.CArr as X
import Language.Alg as X
import Control.Monad.CGen
import Language.SPar
import Language.SPar.Skel.Internal
import Language.SessionTypes.Global ( GT(..) )
import qualified Language.SessionTypes.Common as MPST
import qualified Language.SessionTypes.Global as MPST

--import Debug.Trace
--debug :: String -> CGen a ()
--debug s = trace s $ return ()

newtype AnnStrat = AnnStrat { unAnnStrat :: [AAlg] }

ann :: (CVal a, CVal b) => a :-> b -> AnnStrat
ann f = AnnStrat $ AAlg f : []

instance Semigroup AnnStrat where
  AnnStrat l <> AnnStrat r = AnnStrat $ l ++ r

instance Monoid AnnStrat where
  mempty = AnnStrat []
  mappend = (<>)

inStrat :: (CVal a, CVal b) => a :-> b -> AnnStrat -> Bool
inStrat f x = AAlg f `elem` unAnnStrat x

newtype Defs = Defs { defm :: (Map String AAlg) }

emptyDefs :: Defs
emptyDefs = Defs Map.empty

data St = St { nextPID :: !PID, defs :: !Defs }

emptySt :: St
emptySt = St 1 emptyDefs

incPID :: St -> St
incPID st = st { nextPID = nextPID st + 1 }

-- Reader monad: [Alg] is annotation strategy
-- State monad: next PID
type PM = RWS AnnStrat () St

data SProc v t a = SProc { unProc :: !(Env v t), outR :: !(DType a) }

type CProc a = SProc Alg (:->) a

--generateLibrary :: (CVal a, CVal b)
--                => AnnStrat -> a :=> b -> IO ()
--generateLibrary st f = do
--  pn <- getProgName
--  generateFile (pn ++ ".c") $ genLib f

domTy :: CVal a => t a b -> CTy a
domTy _ = getCTy

codTy :: CVal b => t a b -> CTy b
codTy _ = getCTy

compileAsLib :: (CVal a, CVal b) => String -> AnnStrat -> a :=> b -> CGen ASt ()
compileAsLib fpn st f = do
  mapM_ (uncurry declF) df
  declareEnv fpn p
  !fns <- mapM (wrapParts fpn) pds
  !av <- freshVar
  !ov <- freshVar
  !aty <- cTySpec $ domTy f
  !bty <- cTySpec $ codTy f
  newFun (internalIdent fpn, bty) [(av, aty)]
    $ body fpn av ov bty 0 $ zip pds fns
  newHeaderFun (internalIdent fpn) bty [aty]
  where
    !pds = filter (/= 0) $ domEnvL p
    declF :: String -> AAlg -> CGen ASt ()
    declF fn (AAlg af) = declareFun fn af
    (SProc p _, St _ (Defs (Map.toList -> df)), ()) =
      runRWS (unSkel (f >>> gather 0) $ DVal 0 getCTy) st emptySt

wrapParts :: String -> PID -> CGen ASt Ident
wrapParts fpn p = do
  !fn <- freshN $ "fun_thread_" ++ show p
  !vn <- freshN "arg"
  newFun (fn, ([CTypeSpec $ CVoidType undefNode], [CPtrDeclr [] undefNode]))
    [ (vn, ([CTypeSpec $ CVoidType undefNode], [CPtrDeclr [] undefNode])) ]
    [ CBlockStmt $ cExpr $
      CCall (cVar $ internalIdent $ fpn ++ "_part_" ++ show p) [] undefNode
    , CBlockStmt $ CReturn (Just $ cVar $ internalIdent "NULL") undefNode
    ]
  pure fn


body :: String
     -> Ident
     -> Ident
     -> ([CDeclSpec], [CDerivedDeclr])
     -> PID
     -> [(PID, Ident)]
     -> [CBlockItem]
body fpn av ov (oty, otyd) p0 ps
  = declOut : map declThread ps ++ map createThread ps ++ retn
  ++ map joinThread ps ++ [CBlockStmt $ CReturn (Just $ cVar ov) undefNode]
  where
    declOut = CBlockDecl $ CDecl oty
              [( Just $ CDeclr (Just ov) otyd Nothing [] undefNode
               , Nothing
               , Nothing
               )] undefNode
    retn = [ CBlockStmt $ cExpr $ cAssign (cVar ov) $
             CCall (cVar $ internalIdent $ fpn ++ "_part_" ++ show p0)
             [cVar av] undefNode
           ]
    createThread (p, fn) = CBlockStmt $ cExpr $
      CCall pthreadCreate
      [ cAddr $ cVar $ threadName p
      , cVar $ internalIdent "NULL"
      , cVar fn
      , cVar $ internalIdent "NULL"
      ] undefNode
    joinThread (p, _fn) = CBlockStmt $ cExpr $
      CCall pthreadJoin [cVar $ threadName p, cVar $ internalIdent "NULL"] undefNode
    declThread (p, _) = CBlockDecl $ CDecl
      [CTypeSpec $ CTypeDef (internalIdent "pthread_t") undefNode]
      [mkThreadDeclr p] undefNode
    mkThreadDeclr p =
      ( Just $ CDeclr (Just $ threadName p) [] Nothing [] undefNode
      , Nothing, Nothing)
    threadName p = internalIdent $ "thread" ++ show p
    pthreadCreate = cVar $ internalIdent "pthread_create"
    pthreadJoin = cVar $ internalIdent "pthread_join"

printSProc :: CProc a -> String
printSProc p = printEnv $ unProc p

newtype (:=>) a b = SSkel { unSkel :: DType a -> PM (CProc b) }

annotate :: AnnStrat -> a :=> b -> a :=> b
annotate st (SSkel sk) = SSkel $ \a -> local (\_ -> st) $! sk a

printASkel :: CVal a => a :=> b -> IO ()
printASkel (SSkel f) = do
  putStrLn $ printDefs $ Map.toList $ defm $ defs df
  putStrLn $ printSProc pc
  where
    (pc, df, ()) = runRWS (f $ DVal 0 getCTy) mempty emptySt

    printDefs :: [(String, AAlg)] -> String
    printDefs [] = ""
    printDefs ((d, AAlg e) : es) =
      d ++ " = " ++ printAlg 0 (unFun e) ++ "\n\n" ++ printDefs es

idSkel :: CVal a => a :=> a
idSkel = SSkel $ \x -> pure (SProc emptyEnv x)

pproc :: DType a -> CEnv -> PM (CProc a)
pproc o e = pure $ SProc { unProc = e, outR = o }

pipeline :: a :=> b -> b :=> c -> a :=> c
pipeline (SSkel p) (SSkel q) = SSkel $ \x -> do
  SProc ep op <- p x
  SProc eq oq <- q op
  pproc oq $! kleisliEnv ep eq

instance CCat (:=>) where
  id  = idSkel
  (.) = flip pipeline

anyPID :: DType a -> PID
anyPID i = head $ partsL i

annot :: (CVal a, CVal b) => PID -> a :-> b -> PM PID
annot i f = do
  !b <- reader (f `inStrat`)
  if b then gets nextPID <* modify incPID else pure i

--getName :: a :-> b -> PM String
--getName f = do
--  nms <- Map.keysSet Prelude.. defm <$> gets defs
--  pure $ go nms Nothing
--  where
--    go nms Nothing
--      | nf `Set.member` nms = go nms (Just (0 :: Integer))
--      | otherwise = nf
--    go nms (Just i)
--      |  nfi `Set.member` nms = go nms (Just $ i+1)
--      | otherwise = nfi
--      where
--        nfi = nf ++ "_" ++ show i
--    nf = encName f

--newDef :: (CVal a, CVal b) => a :-> b -> PM String
----newDef (Fun (Prim f _)) = pure f
--newDef f = do
--  df <- gets defs
--  case Map.lookup (AAlg f) (revd df) of
--    Just n -> pure n
--    Nothing -> do
--      nm <- getName f
--      modify $ \s -> s { defs = df { defm = Map.insert nm (AAlg f) $ defm df
--                                   , revd = Map.insert (AAlg f) nm $ revd df
--                                   }
--                       }
--      pure nm

gather :: CVal a => PID -> a :=> a
gather p = SSkel $ \i -> pproc (DVal p getCTy) $ msg i p

gatherNew :: CVal a => a :=> a
gatherNew = SSkel $ \i -> do
  p <- gets nextPID <* modify incPID
  pproc (DVal p getCTy) $ msg i p

instance CArrLift (:->) (:=>) where
  lift f = SSkel $ \i -> do
    p <- annot (anyPID i) f
    pproc (DVal p getCTy) $ kleisliEnv (msg i p) (singleton i p $ \v -> run f v)

constSkel :: (CVal a, CVal b) => a -> b :=> a
constSkel v = sfun $ \_ -> Lit v
--
--   SSkel $ \i -> do
--   let p = anyPID i
--   case projTy i p of
--     ATy ty ->
--       pproc (DVal p getCTy) $
--       singleton ty p $ \_ -> pure (Lit v)

fstSkel :: (CVal a, CVal b) => (a, b) :=> a
fstSkel = SSkel $ \i -> do
  let !ps' = partsL i
      !i' = ifst i
  pproc i' $! efst i ps'

sndSkel :: (CVal a, CVal b) => (a, b) :=> b
sndSkel = SSkel $ \i -> do
  let !ps' = partsL i
      i' = isnd i
  pproc i' $! esnd i ps'

--------------------------------------------------------------------------------
-- REFACTOR BELOW INTO INTERNAL
extendProc :: DType b -> [PID] -> CEnv -> CEnv
extendProc i !ps e = extendEnv pst e
  where
    !pst = map (\p -> let !pr = projTy i p in (p, pr)) ps

agreeDom :: DType c -> CEnv -> CEnv -> CEnv
agreeDom i ea eb =
  case (domEnvL eb ++ partsL i) of
    ps -> extendProc i ps ea
--------------------------------------------------------------------------------

splitProc :: (CVal a, CVal b, CVal c)
          => DType a -> CProc b -> CProc c -> CProc (b, c)
splitProc ir !(SProc el dol) !(SProc er dor) =
  SProc { unProc = splitEnv parts plr prl
        , outR = out
        }
  where
    !parts = participants out
    !plr = agreeDom ir el er
    !prl = agreeDom ir er el
    !out = dPair dol dor
    dPair :: forall a b. (CVal a, CVal b) => DType a -> DType b -> DType (a, b)
    dPair (DVal p1 t1) (DVal p2 t2)
      | p1 Prelude.== p2 = DVal p1 (CPair t1 t2)
    dPair l r = DPair l r

splitSkel :: (CVal a, CVal b, CVal c) => a :=> b -> a :=> c -> a :=> (b, c)
splitSkel (SSkel f) (SSkel g) = SSkel $! \i -> splitProc i <$!> f i <*> g i

instance CArr (:=>) where
  arr nm f = liftAlg (arr nm f)
  lit l = constSkel l
  fst = fstSkel
  snd = sndSkel
  f *** g = (f . fst) &&& (g . snd)
  (&&&) = splitSkel

inlSkel :: forall a b. (CVal a, CVal b) => a :=> Either a b
inlSkel = SSkel $ \i -> pproc (DTagL i getCTy) emptyEnv

inrSkel :: forall a b. (CVal a, CVal b) => b :=> Either a b
inrSkel = SSkel $ \i -> pproc (DTagR getCTy i) emptyEnv

updatePID :: MonadState St m => PID -> m ()
updatePID p = get >>= \x -> put x { nextPID = p }

freezePIDs :: MonadState St m => m a -> m a
freezePIDs m = gets nextPID >>= (m <*) Prelude.. updatePID

parState :: MonadState St m => m a -> m a -> m (a, a)
parState m1 m2 = do
  (x1, s1) <- freezePIDs ((,) <$!> m1 <*> gets nextPID)
  (x2, s2) <- (,) <$!> m2 <*> gets nextPID
  updatePID (max s1 s2) *> pure (x1, x2)

caseSkel :: forall a b c. (CVal a, CVal b, CVal c)
         => a :=> c -> b :=> c -> Either a b :=> c
caseSkel sf@(SSkel f) sg@(SSkel g) = SSkel $ \i ->
  case i of
    DTagL l _   -> f l
    DTagR _ r   -> g r
    DAlt  p l r -> do
      let !(SSkel sc) = caseSkel sf sg
      (el, er) <- parState (sc l) (sc r)
      pure $ choice p l r el er
    DVal p (CEither l r) -> do
      let !j = DAlt p (DTagL (DVal p l) getCTy) (DTagR getCTy (DVal p r))
          !(SSkel sc) = caseSkel sf sg
      sc j

choice :: (CVal a, CVal c)
       => PID
       -> DType a
       -> DType a
       -> CProc c
       -> CProc c
       -> CProc c
choice p il ir (SProc l lo) (SProc r ro) =
  SProc { unProc = kleisliEnv (choiceEnv p il ir ps) e
        , outR = o
        }
  where
    e = caseEnv ps (kleisliEnv l $ einl lo ro po) (kleisliEnv r $ einr lo ro po)
    o = DAlt p lo ro
    po = participants o
    ps = domEnv l `Set.union` domEnv r

instance CArrChoice (:=>) where
  inl = inlSkel
  inr = inrSkel
  f +++ g = (inl . f) ||| (inr . g)
  f ||| g = caseSkel f g
  distrL = skelDistL

-- TODO: check if correct!
skelDistL :: forall a b c. (CVal a, CVal b, CVal c)
         => (Either a b, c) :=> Either (a, c) (b, c)
skelDistL = SSkel $ \i ->
  case i of
    DVal p _ -> pproc (DVal p getCTy) $ singleton i p $ \v -> run distrL v
    DAlt p l r -> do
      (el, er) <- parState (unSkel skelDistL l) (unSkel skelDistL r)
      pure $ choice p l r el er
    DPair (DTagL l _) r -> pproc (DTagL (DPair l r) getCTy) $ emptyEnv
    DPair (DTagR _ l) r -> pproc (DTagR getCTy (DPair l r)) $ emptyEnv
    DPair (DAlt p l r) s -> do
      (el, er) <- parState (unSkel skelDistL (DPair l s)) (unSkel skelDistL (DPair r s))
      pure $ choice p l r el er
    DPair (DVal p (CEither tl tr)) r -> do
      let j = DPair (DAlt p (DTagL (DVal p tl) getCTy) (DTagR getCTy (DVal p tr))) r
      unSkel skelDistL j

sif :: CVal a => (Bool, a) :=> Either a a
sif = lift mif

instance CArrIf (:=>) where
  ifThenElse test l r = test &&& id >>> sif >>> l ||| r

sfun :: (CVal a, CVal b) => (Alg a -> Alg b) -> a :=> b
sfun f = lift $ fun f

instance (CVal a, CVal b, Num b) => Num (a :=> b) where
  f + g = (f &&& g) >>> (sfun $ \v -> afst v + asnd v)
  f * g = (f &&& g) >>> (sfun $ \v -> afst v * asnd v)
  abs f = f >>> (sfun $ \v -> abs v)
  signum f = f >>> (sfun $ \v -> signum v)
  negate f = f >>> (sfun $ \v -> negate v)
  fromInteger i = sfun $ \_ -> fromInteger i

instance (CVal a, CVal b, Num b) => Fractional (a :=> b) where
  f / g = (f &&& g) >>> (sfun $ \v -> afst v / asnd v)
  recip x = 1 / x
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

liftAlg :: (CVal a, CVal b, CArrLift (:->) f) => a :-> b -> f a b
liftAlg = lift

instance CArrCmp (:=>) where
  f < g = f &&& g >>>  (liftAlg $ fst X.<  snd)
  f <= g = f &&& g >>> (liftAlg $ fst X.<= snd)
  f >= g = f &&& g >>> (liftAlg $ fst X.>= snd)
  f > g = f &&& g >>>  (liftAlg $ fst X.>  snd)
  f == g = f &&& g >>> (liftAlg $ fst X.== snd)

-- FIXME: sequential so far
instance CArrVec (:=>) where
  proj = sfun $ \v -> Proj (afst v) (asnd v)
  vsize = sfun $ \v -> VLen v
  vec _f = undefined
  vtake = sfun (\v -> VTake (afst v) (asnd v))
  vdrop = sfun (\v -> VDrop (afst v) (asnd v))


instance CArrFix (:=>) where
  fix f = liftAlg $ fix f
  kfix k f
    | k Prelude.<= 0 = lift (Fun $ Fix f)
  kfix k f = f (kfix (k-1) f)

instance CArrPar (:=>) where
  newProc = gatherNew
  runAt p = gather p

--------------------------------------------------------------------------------
-- Association with global types

-- Global type definitions
type TGT = GT ECTy
data OGT b = OGT { unOGT :: TGT -> TGT, outI :: !(DType b) }
newtype (:~>) a b = Gti { unGti :: DType a -> PM (OGT b) }

annGt :: AnnStrat -> a :~> b -> a :~> b
annGt st (Gti sk) = Gti $ \a -> local (\_ -> st) $! sk a

printGt :: CVal a => a :~> b -> IO ()
printGt (Gti f) = do
  putStrLn $ printDefs $ Map.toList $ defm $ defs df
  MPST.printGT (unOGT pc GEnd)
  where
    (pc, df, ()) = runRWS (f $ DVal 0 getCTy) mempty emptySt

    printDefs :: [(String, AAlg)] -> String
    printDefs [] = ""
    printDefs ((d, AAlg e) : es) =
      d ++ " = " ++ printAlg 0 (unFun e) ++ "\n\n" ++ printDefs es

idGt :: CVal a => a :~> a
idGt = Gti $ \x -> pure (OGT (\y -> y) x)

pipeGt :: a :~> b -> b :~> c -> a :~> c
pipeGt (Gti p) (Gti q) = Gti $ \x -> do
  OGT ep op <- p x
  OGT eq oq <- q op
  pure $! OGT (ep Prelude.. eq) oq

instance CCat (:~>) where
  id  = idGt
  (.) = flip pipeGt

comm :: MPST.Role -> MPST.Role -> ECTy -> TGT -> TGT
comm p q ty (ND l r) = ND (comm p q ty l) (comm p q ty r)
comm p q ty k = Comm p q ty k

msgGt :: DType a -> PID -> TGT -> TGT
msgGt (DVal pt t) p
  | p Prelude.== pt = \k -> k
  | otherwise = comm (MPST.Rol pt) (MPST.Rol p) (eraseTy t)
msgGt (DTagL l _) p = msgGt l p
msgGt (DTagR _ r) p = msgGt r p
msgGt (DPair l r) p = \k -> msgGt l p (msgGt r p k)
msgGt int@(DAlt pc li ri) p
  | p `Set.notMember` ps = \k -> ND (tagL $ msgGt li p k) (tagR $ msgGt ri p k)
  | otherwise = \k -> ND (msgGt li p k) (msgGt ri p k)
  where
    !ps = participants int
    tagL = choiceL (MPST.Rol pc) (MPST.Rol p)
    tagR = choiceR (MPST.Rol pc) (MPST.Rol p)

choiceL :: MPST.Role -> MPST.Role -> TGT -> TGT
choiceL p q (ND l _) = ChoiceL p q l
choiceL p q k = ChoiceL p q k

choiceR :: MPST.Role -> MPST.Role -> TGT -> TGT
choiceR p q (ND _ r) = ChoiceR p q r
choiceR p q k = ChoiceR p q k


gatherGt :: CVal a => PID -> a :~> a
gatherGt p = Gti $ \i -> pure $! OGT (msgGt i p) (DVal p getCTy)

gatherNewGt :: CVal a => a :~> a
gatherNewGt = Gti $ \i -> do
  p <- gets nextPID <* modify incPID
  pure $ OGT (msgGt i p) (DVal p getCTy)

liftGt :: (CVal a, CVal b) => a :-> b -> a :~> b
liftGt f = Gti $ \i -> do
  p <- annot (anyPID i) f
  pure $ OGT (msgGt i p) (DVal p getCTy)

constGt :: (CVal a, CVal b) => a -> b :~> a
constGt v = liftGt $ fun $ \_ -> Lit v

fstGt :: (CVal a, CVal b) => (a, b) :~> a
fstGt = Gti $ \i -> do
  let !i' = ifst i
  pure $ OGT (\k -> k) i'

sndGt :: (CVal a, CVal b) => (a, b) :~> b
sndGt = Gti $ \i -> do
  let i' = isnd i
  pure $ OGT (\k -> k) i'

splitGt :: (CVal a, CVal b, CVal c) => a :~> b -> a :~> c -> a :~> (b, c)
splitGt (Gti f) (Gti g) = Gti $! \i -> do
  OGT g1 o1 <- f i
  OGT g2 o2 <- g i
  pure $ OGT (\k -> g1 (g2 k)) (dPair o1 o2)
  where
    dPair :: forall a b. (CVal a, CVal b) => DType a -> DType b -> DType (a, b)
    dPair (DVal p1 t1) (DVal p2 t2)
      | p1 Prelude.== p2 = DVal p1 (CPair t1 t2)
    dPair l r = DPair l r

instance CArr (:~>) where
  arr nm f = liftGt (arr nm f)
  lit l = constGt l
  fst = fstGt
  snd = sndGt
  f *** g = (f . fst) &&& (g . snd)
  (&&&) = splitGt

caseGt :: forall a b c. (CVal a, CVal b, CVal c)
         => a :~> c -> b :~> c -> Either a b :~> c
caseGt sf@(Gti f) sg@(Gti g) = Gti $ \i ->
  case i of
    DTagL l _   -> f l
    DTagR _ r   -> g r
    DAlt  p l r -> do
      let !(Gti sc) = caseGt sf sg
      (el, er) <- parState (sc l) (sc r)
      pure $ choiceGt p el er
    DVal p (CEither l r) -> do
      let !j = DAlt p (DTagL (DVal p l) getCTy) (DTagR getCTy (DVal p r))
          !(Gti sc) = caseGt sf sg
      sc j

-- choiceGt :: (CVal a, CVal c)
--        => PID
--        -> DType a
--        -> DType a
--        -> OGT c
--        -> OGT c
--        -> OGT c
-- choiceGt p il ir (OGT l lo) (OGT r ro) =
--   OGT { unOGT = cChoice p pkleisliEnv (choiceEnv p il ir ps) e
--       , outI = o
--       }
--   where
--     e = caseGt ps (kleisliEnv l $ einl lo ro po) (kleisliEnv r $ einr lo ro po)
--     o = DAlt p lo ro
--     po = participants o
--     ps = domEnv l `Set.union` domEnv r

-- TODO: check if correct!
gtDistL :: forall a b c. (CVal a, CVal b, CVal c)
         => (Either a b, c) :~> Either (a, c) (b, c)
gtDistL = Gti $ \i ->
  case i of
    DVal p _ -> pure $ OGT (\k -> k) (DVal p getCTy)
    DAlt p l r -> do
      (el, er) <- parState (unGti gtDistL l) (unGti gtDistL r)
      pure $ choiceGt p el er
    DPair (DTagL l _) r -> pure $ OGT (\k -> k) (DTagL (DPair l r) getCTy)
    DPair (DTagR _ l) r -> pure $ OGT (\k -> k) (DTagR getCTy (DPair l r))
    DPair (DAlt p l r) s -> do
      (el, er) <- parState (unGti gtDistL (DPair l s)) (unGti gtDistL (DPair r s))
      pure $ choiceGt p el er
    DPair (DVal p (CEither tl tr)) r -> do
      let j = DPair (DAlt p (DTagL (DVal p tl) getCTy) (DTagR getCTy (DVal p tr))) r
      unGti gtDistL j

choiceGt :: CVal c
         => PID
         -> OGT c
         -> OGT c
         -> OGT c
choiceGt p (OGT l lo) (OGT r ro) =
  OGT (cChoice p (Set.toList (MPST.Rol p `Set.delete` ps)) l r) o
  where
    o = DAlt p lo ro
    ps = MPST.roles (l GEnd) `Set.union` MPST.roles (r GEnd)

cChoice :: PID
        -> [MPST.Role] -> (TGT -> TGT) -> (TGT -> TGT) -> TGT -> TGT
cChoice _ [] _ _ = error "impossible choice"
cChoice p (q:qs) l r = \k ->
  case k of
    ND lk rk -> Choice rp q (cAll choiceL l qs lk) (cAll choiceR r qs rk)
    _ -> Choice rp q (cAll choiceL l qs k) (cAll choiceR r qs k)
  where
    rp = MPST.Rol p
    cAll _ g [] k = g k
    cAll f g (s:rs) k = f rp s (cAll f g rs k)

inlGt :: forall a b. (CVal a, CVal b) => a :~> Either a b
inlGt = Gti $ \i -> pure $ OGT (\k -> k) (DTagL i getCTy)

inrGt :: forall a b. (CVal a, CVal b) => b :~> Either a b
inrGt = Gti $ \i -> pure $ OGT (\k -> k) (DTagR getCTy i)

instance CArrChoice (:~>) where
  inl = inlGt
  inr = inrGt
  f +++ g = (inl . f) ||| (inr . g)
  f ||| g = caseGt f g
  distrL = gtDistL

gif :: CVal a => (Bool, a) :~> Either a a
gif = liftGt mif

instance CArrIf (:~>) where
  ifThenElse test l r = test &&& id >>> gif >>> l ||| r

gfun :: (CVal a, CVal b) => (Alg a -> Alg b) -> a :~> b
gfun f = liftGt $ fun f

instance (CVal a, CVal b, Num b) => Num (a :~> b) where
  f + g = (f &&& g) >>> (gfun $ \v -> afst v + asnd v)
  f * g = (f &&& g) >>> (gfun $ \v -> afst v * asnd v)
  abs f = f >>> (gfun $ \v -> abs v)
  signum f = f >>> (gfun $ \v -> signum v)
  negate f = f >>> (gfun $ \v -> negate v)
  fromInteger i = gfun $ \_ -> fromInteger i

instance (CVal a, CVal b, Num b) => Fractional (a :~> b) where
  f / g = (f &&& g) >>> (gfun $ \v -> afst v / asnd v)
  recip x = 1 / x
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

instance CArrCmp (:~>) where
  f < g = f &&& g >>>  (liftGt $ fst X.<  snd)
  f <= g = f &&& g >>> (liftGt $ fst X.<= snd)
  f >= g = f &&& g >>> (liftGt $ fst X.>= snd)
  f > g = f &&& g >>>  (liftGt $ fst X.>  snd)
  f == g = f &&& g >>> (liftGt $ fst X.== snd)

-- FIXME: sequential so far
instance CArrVec (:~>) where
  proj = gfun $ \v -> Proj (afst v) (asnd v)
  vsize = gfun $ \v -> VLen v
  vec _f = undefined
  vtake = gfun (\v -> VTake (afst v) (asnd v))
  vdrop = gfun (\v -> VDrop (afst v) (asnd v))

instance CArrFix (:~>) where
  fix f = liftGt $ fix f
  kfix k f
    | k Prelude.<= 0 = liftGt (Fun $ Fix f)
  kfix k f = f (kfix (k-1) f)

instance CArrPar (:~>) where
  newProc = gatherNewGt
  runAt p = gatherGt p

instance CArrLift (:->) (:~>) where
  lift = liftGt


generateGtFile :: FilePath -> [(String, TGT)] -> IO ()
generateGtFile fp ms =
  writeFile (fp ++ ".mpst") $ mpstf ++ "\n"
  where
    mpstf = concat (intersperse "\n\n" $ map pprSt ms)
    pprSt (f, g) = f ++ " ::: " ++ show (pretty g)

inferGt :: (CVal a, CVal b) => AnnStrat -> a :~> b -> TGT
inferGt st f = p
  where
    p = pp GEnd
    (OGT pp _, _, ()) =
      runRWS (unGti (f >>> gatherGt 0) $ DVal 0 getCTy) st emptySt
