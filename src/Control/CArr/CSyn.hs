{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{- LANGUAGE StandaloneDeriving #-}
{- LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeFamilies #-}
{- LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.CArr.CSyn
  ( Var
  , Expr(..)
  , (:<:)(..)
  , (X..)
  , vlet
  , (.$)
  , fix
  , pfix
--  , Fun
  , prim
  , primLit
  , cfun
  , app
  , pair
  , fst
  , snd
  , acase
  , (.|)
  , (.||)
  , inl
  , inr
  , X.ifThenElse
  , vget
  , vdrop
  , vtake
  , vsize
  , par
  , (@@)
  , (Prelude.$)
  , IsSing
  , FromNat
  , X.CArrCmp(..)
  , Prelude.Num(..)
  , Prelude.Fractional(..)
  , CAlg
  , CVal
  , X.CArrFix
  , Either
  , Int
  , Prelude.Double
  , Prelude.Integer
  , Prod
  , TProd
  , INat(..)
  , SINat
  , Sing(..)
  , CValProd
  , CValTProd
  , pmap
  , smap
  , pfold
  , sfold
  , ssplit
  , psplit
  , psplitv
  , ssplitv
  , splitv
  , pzip
  , withSize
  , fromINat
  , toInteger
  , Tree
  -- , tsplit
  , zipTree

  , CDict
  , cdictTree
  , cdictProd
  , withCDict
) where

import qualified Prelude

import Data.C
import qualified Control.CCat as X
import qualified Control.CArr as X
import Control.CArr ( CAlg )

import GHC.TypeLits

class ctx :<: ctx' where
  sub :: CAlg t => Expr t ctx' ctx

--instance {-# OVERLAPPING #-}
--  (CVal ctx) => ctx :<: ctx where
--  sub = X.id
--
--instance
--  (CVal a, ctx :<: ctx') => ctx :<: (a, ctx') where
--  sub = X.snd X.>>> sub

type family (:==:) a b where
  a :==: a = 'Prelude.True
  _ :==: _ = 'Prelude.False

class (CVal ctx, CVal ctx', ctx :==: ctx' ~ flag) => Sub flag ctx ctx' where
  subCtx :: CAlg t => t ctx' ctx

instance CVal ctx => Sub 'Prelude.True ctx ctx where
  subCtx = X.id

instance (CVal a, Sub b ctx1 ctx2, (ctx1 :==: (a, ctx2)) ~ 'Prelude.False) =>
  Sub 'Prelude.False ctx1 (a, ctx2) where
  subCtx = X.snd X.>>> subCtx

instance (CVal ctx, CVal ctx', Sub b ctx ctx') => ctx :<: ctx' where
  sub = Expr subCtx

--instance
--  (CVal ctx, CVal ctx'', ctx :<: ctx', ctx' :<: ctx'') =>
--  ctx :<: ctx'' where
--  sub = sub X.>>> sub @ctx @ctx'

--newtype Var ctx a = Var { unVar :: forall t. CAlg t => t ctx a }
--
--var :: (CAlg t, CVal a, ctx :<: ctx') => Var ctx a -> t ctx' a
--var v = sub X.>>> unVar v

type Var t ctx a = forall ctx'. (CVal ctx', ctx :<: ctx') => Expr t ctx' a

data Expr t ctx a where
  Expr :: (CVal ctx, CVal a) => t ctx a -> Expr t ctx a

instance X.CCat t => X.CCat (Expr t) where
  Expr f . Expr g = Expr (f X.. g)
  id = Expr X.id

instance X.CArr t => X.CArr (Expr t) where
  arr n v = Expr (X.arr n v)
  lit l = Expr (X.lit l)
  fst = Expr X.fst
  snd = Expr X.snd
  first (Expr f) = Expr (X.first f)
  second (Expr f) = Expr (X.second f)
  Expr f &&& Expr g = Expr (f X.&&& g)
  Expr f *** Expr g = Expr (f X.*** g)

instance X.CArrChoice t => X.CArrChoice (Expr t) where
  inl = Expr X.inl
  inr = Expr X.inr
  left (Expr f)  = Expr (X.left f)
  right (Expr f) = Expr (X.right f)
  Expr f +++ Expr g = Expr (f X.+++ g)
  Expr f ||| Expr g = Expr (f X.||| g)
  distrL = Expr X.distrL

instance X.CArrIf t => X.CArrIf (Expr t) where
  ifThenElse (Expr b) (Expr l) (Expr r) = Expr (X.ifThenElse b l r)

instance (CVal a, CVal b, Prelude.Num (f a b)) => Prelude.Num (Expr f a b) where
  Expr f + Expr g = Expr (f Prelude.+ g)
  Expr f * Expr g = Expr (f Prelude.* g)
  abs (Expr f) = Expr (Prelude.abs f)
  signum (Expr f) = Expr (Prelude.signum f)
  negate (Expr f) =  Expr (Prelude.negate f)
  fromInteger i = Expr (Prelude.fromInteger i)

instance (CVal a, CVal b, Prelude.Num b, Prelude.Fractional (f a b)) =>
  Prelude.Fractional (Expr f a b) where
  Expr f / Expr g = Expr (f Prelude./ g)
  recip (Expr x) = Expr (1 Prelude./ x)
  fromRational x = Expr (Prelude.fromRational x)

instance X.CAp f v => X.CAp (Expr f) v where
  ap (Expr f) v = X.ap f v

instance X.CArrCnst f v => X.CArrCnst (Expr f) v where
  const e = Expr (X.const e)

instance X.CArrCmp f => X.CArrCmp (Expr f) where
  Expr f <  Expr g = Expr (f X.< g)
  Expr f <= Expr g = Expr (f X.<= g)
  Expr f >= Expr g = Expr (f X.>= g)
  Expr f >  Expr g = Expr (f X.> g)
  Expr f == Expr g = Expr (f X.== g)

instance X.CArrVec f => X.CArrVec (Expr f) where
  proj =  Expr X.proj
  vec (Expr f) = Expr (X.vec f)
  vsize = Expr X.vsize
  vtake = Expr X.vtake
  vdrop = Expr X.vdrop

instance X.CArrPar f => X.CArrPar (Expr f) where
  newProc =  Expr X.newProc
  runAt p = Expr (X.runAt p)

instance X.CArrFix f => X.CArrFix (Expr f) where
  fix f = Expr (X.fix f)
  kfix n f = Expr (X.kfix n f)

fix :: (CAlg t, X.CArrFix t, CVal b, CVal a)
    => Int
    -> (forall f. CAlg f => (forall ctx. Expr f ctx a -> Expr f ctx b) ->
        Var f a a -> Expr f a b)
    -> t a b
fix k f = X.kfix k Prelude.$ \rf ->
  case f (app (Expr rf)) sub of
    Expr o -> o

pfix :: forall t a b. (CAlg t, X.CArrFix t, CVal b, CVal a)
    => Int
    -> (forall f. (CAlg f, X.CArrFix f) => (forall ctx. Expr f ctx a -> Expr f ctx b) ->
        Var f a a -> Expr f a b)
    -> t a b
pfix k f = kfix k Prelude.$ \rf ->
  case f (app (Expr rf)) sub of
    Expr o -> o
  where
    kfix :: (CVal a, CVal b) => Int -> (forall f. CAlg f => f a b -> f a b)
         -> t a b
    kfix kk ff
      | kk Prelude.<= 0 = X.newProc X.>>> X.fix ff
    kfix kk ff = ff (kfix (kk Prelude.- 1) ff)

cfun :: CAlg t => (Var t a a -> Expr t a b) -> t a b
cfun f = case f sub of
           Expr e -> e

app :: CAlg t
    => Expr t a b -> Expr t ctx a -> Expr t ctx b
app (Expr f) (Expr x) =  Expr (f X.. x)

prim :: (CAlg t, CVal b) => String -> Expr t ctx a -> Expr t ctx b
prim s (Expr x) = Expr Prelude.$ X.arr s Prelude.undefined X.. x

primLit :: (CAlg t, CVal a, CVal ctx) => a -> Expr t ctx a
primLit s = Expr (X.lit s)

vlet :: forall a t ctx b. CAlg t
     => Expr t ctx a -> (CVal a => Var t (a, ctx) a -> Expr t (a, ctx) b) -> Expr t ctx b
vlet (Expr x) f =
  case f fstCtx of
    Expr fx -> Expr (x X.&&& X.id X.>>> fx)
  where
    fstCtx :: forall ctx'. (CVal ctx', (a, ctx) :<: ctx') => Expr t ctx' a
    fstCtx = case (sub :: Expr t ctx' (a, ctx)) of
               Expr ff -> Expr (ff X.>>> (X.fst :: t (a, ctx) a))

(.$) :: CAlg t
     => (Var t (a, ctx) a -> Expr t (a, ctx) b) -> Expr t ctx a -> Expr t ctx b
f .$ x = vlet x f

pair :: CAlg t
     => (Expr t ctx a, Expr t ctx b) -> Expr t ctx (a, b)
pair (Expr l, Expr r) = Expr (l X.&&& r)

fst :: (CAlg t, CVal a, CVal b)
     => Expr t ctx (a, b) -> Expr t ctx a
fst (Expr f) = Expr (f X.>>> X.fst)

snd :: (CAlg t, CVal a, CVal b)
     => Expr t ctx (a, b) -> Expr t ctx b
snd (Expr f) = Expr (f X.>>> X.snd)

docase :: (CAlg t, CVal a, CVal b)
       => Expr t ctx (Either a b) -> Expr t ctx (Either (a, ctx) (b, ctx))
docase (Expr f) = Expr (f X.&&& X.id X.>>> X.distrL)

(>>>) :: CAlg t => Expr t a b -> Expr t b c -> Expr t a c
Expr l >>> Expr r = Expr (l X.>>> r)

(|||) :: CAlg t => Expr t a c -> Expr t b c -> Expr t (Either a b) c
Expr l ||| Expr r = Expr (l X.||| r)

acase :: forall t ctx a b c. (CAlg t, CVal a, CVal b)
      => Expr t ctx (Either a b)
      -> (Var t (a, ctx) a -> Expr t (a, ctx) c)
      -> (Var t (b, ctx) b -> Expr t (b, ctx) c)
      -> Expr t ctx c
acase x@Expr{} l r = docase x >>> (l fstCtx ||| r fstCtx)
  where
    fstCtx :: forall aa ctx'. (CVal aa, CVal ctx', (aa, ctx) :<: ctx')
           => Expr t ctx' aa
    fstCtx = case (sub :: Expr t ctx' (aa, ctx)) of
               Expr f -> Expr (f X.>>> (X.fst :: t (aa, ctx) aa))
  -- l (fst sub) |||
  -- r (fst sub)

(.|) :: ( (Var t (a, ctx) a -> Expr t (a, ctx) c) ->
          (Var t (b, ctx) b -> Expr t (b, ctx) c) ->
          Expr t ctx c )
     -> (Var t (a, ctx) a -> Expr t (a, ctx) c)
     -> (Var t (b, ctx) b -> Expr t (b, ctx) c)
     -> Expr t ctx c
cse .| f = cse f

(.||) :: ( (Var t (b, ctx) b -> Expr t (b, ctx) c) ->
           Expr t ctx c )
     -> (Var t (b, ctx) b -> Expr t (b, ctx) c)
     -> Expr t ctx c
cse .|| f = cse f

inl :: (CAlg t, CVal b) => Expr t ctx a -> Expr t ctx (Either a b)
inl x@Expr{} = x >>> Expr X.inl

inr :: (CAlg t, CVal a) => Expr t ctx b -> Expr t ctx (Either a b)
inr x@Expr{} = x >>> Expr X.inr


vget :: (CAlg t, CVal a) => Expr t ctx Int -> Expr t ctx [a] -> Expr t ctx a
vget (Expr i) (Expr v) = Expr (i X.&&& v X.>>> X.proj)

vtake :: (CAlg t, CVal a) => Expr t ctx Int -> Expr t ctx [a] -> Expr t ctx [a]
vtake (Expr i) (Expr v) = Expr (i X.&&& v X.>>> X.vtake)

vdrop :: (CAlg t, CVal a) => Expr t ctx Int -> Expr t ctx [a] -> Expr t ctx [a]
vdrop (Expr i) (Expr v) = Expr (i X.&&& v X.>>> X.vdrop)

vsize :: (CAlg t, CVal a) => Expr t ctx [a] -> Expr t ctx Int
vsize (Expr v) = Expr (v X.>>> X.vsize)

par :: CAlg t
    => (Var t ctx a -> Expr t ctx b) -> Expr t ctx a -> Expr t ctx b
par f x@Expr{} = f (sub >>> x >>> Expr X.newProc)

(@@) :: CAlg t => (Var t ctx a -> Expr t ctx b) -> Prelude.Integer
     -> Expr t ctx a -> Expr t ctx b
f @@ p = \x@Expr{} -> f (sub >>> x >>> Expr (X.runAt p))

---- Pairs

data INat = Z | S INat

data family Sing :: k -> *

data instance Sing (n :: INat) where
  SZ :: Sing 'Z
  SS :: Sing n -> Sing ('S n)

class IsSing a where sing :: Sing a
instance IsSing 'Z where sing = SZ
instance IsSing n => IsSing ('S n) where sing = SS sing
type SINat (n :: INat) = Sing n

fromINat :: Prelude.Num a => SINat n -> a
fromINat = Prelude.fromInteger Prelude.. toInteger

toInteger :: SINat n -> Prelude.Integer
toInteger SZ = 0
toInteger (SS n) = 1 Prelude.+ toInteger n

type IsNat (n :: INat) = IsSing n

data SomeINat where
  SomeINat :: forall n. IsNat n => SINat n -> SomeINat

fromInteger :: Prelude.Integer -> SomeINat
fromInteger i | i Prelude.<= 0 = SomeINat SZ
fromInteger i = case fromInteger (i Prelude.- 1) of
                  SomeINat n -> SomeINat (SS n)

withSize :: Prelude.Integer -> (forall n. IsSing n => SINat n -> a) -> a
withSize i f = case fromInteger (i Prelude.- 1) of
                 SomeINat n -> f n

type TProd n a = Prod (FromNat n) a

type family Prod (n :: INat) (a :: *) = r where
  Prod 'Z a = a
  Prod ('S n) a = (a, Prod n a)

data CDict a where
  CDict :: CVal a => CDict a

cdictProd :: forall a n. CVal a => SINat n -> CDict (Prod n a)
cdictProd SZ = CDict
cdictProd (SS m) = case cdictProd @a m of
                    CDict -> CDict

fmapP' :: forall t a b ctx n v. (CAlg t, CVal a)
       => SINat n -> v a -> v b -> Bool
       -> (Expr t ctx a -> Expr t ctx b)
       -> Expr t ctx (Prod n a) -> Expr t ctx (Prod n b)
fmapP' SZ _ _ b f x
  | b = f (x >>> Expr X.newProc)
  | Prelude.otherwise  = f x
fmapP' (SS m) tya tyb b f x =
  case cdictProd @a m of
    CDict ->
      let fx@Expr{} = if b then f (fst x >>> Expr X.newProc)
                      else f (fst x)
      in pair (fx, fmapP' m tya tyb b f (snd x))


--fmapPIx :: SINat n -> (Int -> a -> b) -> Int -> Prod n a -> Prod n b
--fmapPIx SZ _ _ = const unit
--fmapPIx (SS m) f k = f k *** fmapPIx m f (k+1)

pmap :: forall a b n t ctx.
        (CAlg t, CVal a)
     => SINat n
     -> (Expr t ctx a -> Expr t ctx b)
     -> Expr t ctx (Prod n a) -> Expr t ctx (Prod n b)
pmap n = fmapP' n Proxy Proxy Prelude.True

smap :: forall a b n t ctx.
        (CAlg t, CVal a)
     => SINat n
     -> (Expr t ctx a -> Expr t ctx b)
     -> Expr t ctx (Prod n a) -> Expr t ctx (Prod n b)
smap sn = fmapP' sn Proxy Proxy Prelude.False

type CValProd n a = (IsSing n, CVal (Prod n a))
type CValTProd n a = (IsSing (FromNat n), CVal (Prod (FromNat n) a))

--assocL :: (CAlg t, CVal a, CVal b, CVal c) => t (a, (b, c)) ((a, b), c)
--assocL = cfun Prelude.$ \x -> pair (pair (fst x, fst (snd x)), snd (snd x))

type family Div2 (n :: INat) where
  Div2 'Z = 'Z
  Div2 ('S 'Z) = 'Z
  Div2 ('S ('S n)) = 'S (Div2 n)

div2 :: SINat n -> SINat (Div2 n)
div2 SZ = SZ
div2 (SS SZ) = SZ
div2 (SS (SS n)) = SS (div2 n)

data Proxy a = Proxy

(&&&) :: CAlg t => Expr t a b -> Expr t a c -> Expr t a (b,c)
Expr f &&& Expr g = Expr (f X.&&& g)

red :: forall t a n. CAlg t
    => SINat n -> Expr t (a, a) a
    -> Expr t (Prod n a) (Prod (Div2 n) a)
red SZ Expr{} = Expr X.id
red (SS SZ) f = f
red (SS (SS n)) f@Expr{} =
  case (cdictProd @a n, cdictProd @a (div2 n)) of
    (CDict, CDict) ->
      ((ffst &&& (fsnd >>> ffst)) >>> f) &&& ((fsnd >>> fsnd) >>> red n f)
  where
    ffst :: (CVal a, CVal b) => Expr t (a, b) a
    ffst = Expr X.fst
    fsnd :: (CVal a, CVal b) => Expr t (a, b) b
    fsnd = Expr X.snd

pfold' :: forall t n a. CAlg t
       => SINat n -> Expr t (a, a) a
       -> Expr t (Prod n a) a
pfold' SZ Expr{} = Expr X.id
pfold' (SS SZ) f = f
pfold' n f@Expr{} =
  case (cdictProd @a n, cdictProd @a dn2) of
    (CDict, CDict) -> red n f >>> pfold' dn2 f
  where
    !dn2 = div2 n

pfold :: forall t n a ctx. CAlg t
       => SINat n -> Expr t (a, a) a
       -> Expr t ctx (Prod n a) -> Expr t ctx a
pfold n f x = x >>> pfold' n f

sfold :: forall t n a ctx. (CAlg t, CVal a)
      => SINat n -> (Expr t ctx (a, a) -> Expr t ctx a)
      -> Expr t ctx (Prod n a) -> Expr t ctx a
sfold SZ _  x = x
sfold (SS n) f x =
  case cdictProd @a n of
    CDict -> f (pair (fst x, sfold n f (snd x)))

--sfold :: forall n t a ctx. (CAlg t, CVal a, CVal ctx, IsSing (FromNat n))
--      => (t ctx (a, a) -> t ctx a) -> t ctx (Prod (FromNat n) a) -> t ctx a
--sfold = sfold' (sing :: SINat (FromNat n))

split' :: forall t a ctx n. (CAlg t, CVal ctx, CVal a)
       => Bool -> Prelude.Integer -> SINat n
       -> (Expr t ctx Int -> Expr t ctx a)
       -> Expr t ctx (Prod n a)
split' b i SZ g =
  if b then g (Expr (Prelude.fromInteger i)) >>> Expr X.newProc
  else g (Expr (Prelude.fromInteger i))
split' b i (SS n) g =
      let gi = if b then g (Expr (Prelude.fromInteger i)) >>> Expr X.newProc
               else g (Expr (Prelude.fromInteger i))
      in pair (gi, split' b (i Prelude.+ 1) n g)

psplit :: forall n t a ctx. (CAlg t, CVal ctx)
      => SINat n -> (Expr t ctx Int -> Expr t ctx a) -> Expr t ctx (Prod n a)
psplit n f =
  case f (Expr (Prelude.fromInteger 0)) of
    Expr{} -> split' Prelude.True 0 n f

ssplit :: forall a n t ctx. (CAlg t, CVal ctx)
      => SINat n -> (Expr t ctx Int -> Expr t ctx a) -> Expr t ctx (Prod n a)
ssplit n f =
  case f (Expr (Prelude.fromInteger 0)) of
    Expr{} -> split' Prelude.False 0 n f

splitv' :: forall t a n. (CAlg t, CVal a)
        => Bool -> SINat n -> t (Int, [a]) (Prod n [a])
splitv' b SZ =
  if b then X.snd X.>>> X.newProc
  else X.snd
splitv' b (SS n) =
  case cdictProd @[a] n of
    CDict ->
      (if b then X.vtake X.>>> X.newProc else X.vtake)
      X.&&& ((X.fst X.&&& X.vdrop) X.>>> splitv' b n)

psplitv :: forall a t n. (CAlg t, CVal a)
        => SINat n -> t [a] (Prod n [a])
psplitv n =
  case cdictProd @[a] n of
    CDict -> (X.vsize Prelude./ (Prelude.fromInteger isn)) X.&&& X.id
             X.>>> splitv' Prelude.True n
  where
    isn = 1 Prelude.+ toInteger n

ssplitv :: forall a t n. (CAlg t, CVal a)
        => SINat n -> t [a] (Prod n [a])
ssplitv n =
  case cdictProd @[a] n of
    CDict -> (X.vsize Prelude./ (Prelude.fromInteger isn)) X.&&& X.id
             X.>>> splitv' Prelude.False n
  where
    isn = 1 Prelude.+ toInteger n

splitv :: forall n a f ctx. (CAlg f, CVal a, IsSing n)
       => SINat n -> Expr f ctx [a] -> Expr f ctx (Prod n [a])
splitv n x@Expr{} =
  withCDict (cdictProd @[a] n) Prelude.$ app (ssplitv n) x

pzip' :: forall a b n f. (CAlg f, CVal a, CVal b)
       => SINat n -> f (Prod n a, Prod n b) (Prod n (a, b))
pzip' SZ = X.id
pzip' (SS n) =
  case (cdictProd @a n, cdictProd @b n, cdictProd @(a, b) n) of
    (CDict, CDict, CDict) ->
      ((X.fst X.>>> X.fst) X.&&& (X.snd X.>>> X.fst)) X.&&&
      ((X.fst X.>>> X.snd) X.&&& (X.snd X.>>> X.snd) X.>>> pzip' @a @b n)

pzip :: forall a b n f ctx. (CAlg f, CVal a, CVal b)
     => SINat n
     -> Expr f ctx (Prod n a) -> Expr f ctx (Prod n b)
     -> Expr f ctx (Prod n (a, b))
pzip sn l r =
  case (cdictProd @a sn, cdictProd @b sn, cdictProd @(a,b) sn) of
    (CDict, CDict, CDict) -> (l &&& r) >>> Expr (pzip' @a @b sn)

type family ToNat (i :: INat) :: Nat where
  ToNat 'Z = 0
  ToNat ('S n) = 1 + ToNat n

type family FromNat (i :: Nat) :: INat where
  FromNat 0 = 'Z
  FromNat n = 'S (FromNat (n-1))

type family Tree (n :: INat) (a :: *)  where
  Tree 'Z a = a
  Tree ('S n) a = (Tree n a, Tree n a)

cdictTree :: forall a n. CVal a => SINat n -> CDict (Tree n a)
cdictTree SZ = CDict
cdictTree (SS n) = case cdictTree @a n of
                     CDict -> CDict

zipTree' :: CAlg f => SINat n -> f ([Double], [Double]) [Double]
         -> f (Tree n [Double], Tree n [Double]) (Tree n [Double])
zipTree' SZ f = f
zipTree' (SS x) f =
  case cdictTree @[Double] x of
    CDict ->
      let swap = ((X.fst X.>>> X.fst) X.&&& (X.snd X.>>> X.fst)) X.&&&
                 ((X.fst X.>>> X.snd) X.&&& (X.snd X.>>> X.snd))
      in swap X.>>> (zipTree' x f X.*** zipTree' x f)

zipTree :: (CAlg f, CVal ctx)
        => SINat n -> f ([Double], [Double]) [Double]
        -> Expr f ctx (Tree n [Double], Tree n [Double])
        -> Expr f ctx (Tree n [Double])
zipTree n f x =
  case cdictTree @[Double] n of
    CDict -> x X.>>> Expr (zipTree' n f)

withCDict :: CDict a -> (CVal a => b) -> b
withCDict CDict f = f
