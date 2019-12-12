{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wwarn #-}
module Language.Test where

import Control.Arrow
import Data.Complex
import Data.Proxy
import Type.Reflection

import GHC.TypeLits

data INat = Z | S INat

data family Sing :: k -> *

data instance Sing (n :: INat) where
  SZ :: Sing 'Z
  SS :: Sing n -> Sing ('S n)

class IsSing a where sing :: Sing a
instance IsSing 'Z where sing = SZ
instance IsSing n => IsSing ('S n) where sing = SS sing
type SINat (n :: INat) = Sing n

newtype Unit a = MkUnit () deriving Show

unit :: Unit a
unit = MkUnit ()

type family Prod (n :: INat) (a :: *) = r | r -> n a where
  Prod 'Z a = Unit a
  Prod ('S n) a = (a, Prod n a)

fmapP' :: SINat n -> (a -> b) -> Prod n a -> Prod n b
fmapP' SZ     _ = const unit
fmapP' (SS m) f = f{- ALLOCATE HERE -} *** fmapP' m f

fmapPIx :: SINat n -> (Int -> a -> b) -> Int -> Prod n a -> Prod n b
fmapPIx SZ _ _ = const unit
fmapPIx (SS m) f k = f k *** fmapPIx m f (k+1)

fmapP :: IsSing n => (a -> b) -> Prod n a -> Prod n b
fmapP = fmapP' sing

type family Div2 (n :: INat) where
  Div2 'Z = 'Z
  Div2 ('S 'Z) = 'Z
  Div2 ('S ('S n)) = 'S (Div2 n)

div2 :: SINat n -> SINat (Div2 n)
div2 SZ = SZ
div2 (SS SZ) = SZ
div2 (SS (SS n)) = SS (div2 n)

assocL :: (a, (b, c)) -> ((a, b), c)
assocL (x, (y, z)) = ((x, y), z)

red :: SINat n -> ((a, a) -> a) -> Prod n a -> Prod (Div2 n) a
red SZ _ = id
red (SS SZ) _ = snd
red (SS (SS n)) f = assocL >>> f{- ALLOCATE HERE -} *** red n f

fold :: SINat n -> ((a, a) -> a) -> a -> Prod n a -> a
fold SZ _ z = const z
fold (SS SZ) _ _ = fst
fold n f z = red n f >>> fold (div2 n) f z

reduce :: (IsSing n, Monoid a) => Prod n a -> a
reduce = fold sing (uncurry mappend) mempty

data Fin (m :: INat) where
  FZ :: Fin ('S m)
  FS :: Fin m -> Fin ('S m)

data Le (n :: INat) (m :: INat) where
  LZ :: Le 'Z m
  LS :: Le n m -> Le ('S n) ('S m)

class (IsSing n, IsSing m) => IsLe (n :: INat) (m :: INat) where
  le :: Sing n -> Le n m
instance IsSing n => IsLe 'Z n where
  le SZ = LZ
instance IsLe n m => IsLe ('S n) ('S m) where
  le (SS n) = LS (le n)

get' :: Le n m -> Prod ('S m) a -> a
get' LZ     = fst
get' (LS n) = snd >>> get' n

get :: IsLe n m => SINat n -> Prod ('S m) a -> a
get n = get' (le n)

zw' :: forall a b c (n :: INat). SINat n -> ((a, b) -> c) -> (Prod n a, Prod n b) -> Prod n c
zw' SZ     _ = const unit
zw' (SS m) f = swap >>> f *** zw' m f

toNum :: forall (k :: INat) a. Num a => SINat k -> a
toNum SZ = 0
toNum (SS sm) = 1 + toNum sm

expn :: Float -> Float -> Complex Float -> Complex Float
expn n k c = cis (-2 * pi * k /  n) * c {- multiply exponential -}

add :: (Complex Float, Complex Float) -> Complex Float
add = uncurry (+)

sub :: (Complex Float, Complex Float) -> Complex Float
sub = uncurry (-)

type family Add (n :: INat) (m :: INat) where
  Add 'Z n = n
  Add ('S n) m = 'S (Add n m)

addn :: SINat n -> SINat m -> SINat (Add n m)
addn SZ n = n
addn (SS n) m = SS (addn n m)

type family Mul (n :: INat) (m :: INat) where
  Mul 'Z n = 'Z
  Mul ('S n) m = Add m (Mul n m)

mul :: SINat n -> SINat m -> SINat (Mul n m)
mul SZ _ = SZ
mul (SS n) m = addn m (mul n m)

type family Pow2 (n :: INat) where
  Pow2 'Z = 'S 'Z
  Pow2 ('S n) = Add (Pow2 n) (Pow2 n)

pow2 :: SINat n -> SINat (Pow2 n)
pow2 SZ = SS SZ
pow2 (SS n) = addn (pow2 n) (pow2 n)

-- Hack: proper way would be to traverse 'n' and 'm', but it is quite inefficient

szr :: SINat n -> Add n 'Z :~: n
szr SZ = Refl
szr (SS n) = case szr n of
               Refl -> Refl

ssr :: SINat n -> SINat m -> Add n ('S m) :~: 'S (Add n m)
ssr SZ _ = Refl
ssr (SS n) m = case ssr n m of
                 Refl -> Refl

addComm :: SINat n -> SINat m -> Add n m :~: Add m n
addComm SZ m = case szr m of Refl -> Refl -- unsafeCoerce Refl
addComm (SS n) m = case (addComm n m, ssr m n) of
                     (Refl, Refl) -> Refl

--deinterleave
split :: forall a (n :: INat). SINat n -> Prod (Add n n) a -> (Prod n a, Prod n a)
split SZ = id &&& id
split (SS n) =
  case addComm n (SS n) of
    Refl -> assocL >>> id *** split n >>> swap

assocR :: ((a, b), c) -> (a, (b, c))
assocR ((a, b), c) = (a, (b, c))

cat :: SINat n -> (Prod n a, Prod m a) -> Prod (Add n m) a
cat SZ = snd
cat (SS x) = assocR >>> id *** cat x

fromINat :: forall a n. Num a => SINat n -> a
fromINat SZ = 0
fromINat (SS x) = 1 + fromINat x

fromTo :: forall a n. Num a => SINat n -> Prod n a
fromTo = go 0

go :: forall a n. Num a => a -> SINat n -> Prod n a
go _ SZ = unit
go n (SS m) = (n, go (1 + n) m)

dft :: SINat n -> Prod (Pow2 n) (Complex Float) -> Prod (Pow2 n) (Complex Float)
dft SZ = id
dft (SS x)
   = split p2x >>>
     dft x {-EVENS-} *** dft x {-ODDS-} >>>
     id *** fmapPIx p2x (expn p2sx . fromIntegral) 0 {- Multiply by exponential -} >>>
     zw' p2x add {- Left side -} &&& zw' p2x sub {- right side -} >>>
     cat p2x
  where
    p2x = pow2 x
    p2sx :: Float
    p2sx = fromIntegral $ ((2 ^ (fromINat (SS x) :: Integer)) :: Integer)

--------------------------------------------------------------------------------
-- FFT BELOW -------------------------------------------------------------------

-- PRIM FUNCTIONS --------------------------------------------------------------

-- baseFFT is one of your PRIM funcs: you can copy from some online FFT implementation: e.g. Rosetta Code

baseFFT :: RealFloat a => [Complex a] -> [Complex a]
baseFFT [] = []
baseFFT [x] = [x]
baseFFT xs = zipWith (+) ys ts ++ zipWith (-) ys ts
    where n = length xs
          ys = baseFFT evens
          zs = baseFFT odds
          (evens, odds) = splitList xs
          ts = zipWith (\z k -> (exp' k n) * z) zs [0::Integer ..]

exp' :: (Floating a1, Integral a2, Integral a3) => a2 -> a3 -> Complex a1
exp' k n = cis $ -2 * pi * (fromIntegral k) / (fromIntegral n)

splitList :: [a] -> ([a], [a])
splitList [] = ([], [])
splitList [x] = ([x], [])
splitList (x:y:xs) = (x:xt, y:yt) where (xt, yt) = splitList xs

addPadding :: Num a => SINat n -> [a] -> [a]
addPadding sz l = l ++ replicate (padding $ length l) 0
  where
    padding k = 2 ^ (max ((fromINat sz) :: Integer) (ceiling (logBase 2 $ fromIntegral k :: Double))) - k

concatenate :: ([a], [a]) -> [a]
concatenate = uncurry (++)

{- Below should be your prim functions -}
mulExp :: (RealFloat a1, Integral a3) =>
                a3 -> Int -> [Complex a1] -> [Complex a1]
mulExp p2sx i l = zipWith (\k z -> exp' k (p2sx * fromIntegral len) * z) [i * len ..] l
  where
    len = length l
addc :: ([Complex Float], [Complex Float]) -> [Complex Float]
addc = uncurry $ zipWith (+)
subc :: ([Complex Float], [Complex Float]) -> [Complex Float]
subc = uncurry $ zipWith (-)

-- END PRIM FUNCTIONS ----------------------------------------------------------

-- ARROW DEFINITIONS -----------------------------------------------------------
---  Should not change, except types

type family Tree (n :: INat) (a :: *)  where
  Tree 'Z a = a
  Tree ('S n) a = (Tree n a, Tree n a)

splitL :: SINat m -> [Complex Float] -> Tree m [Complex Float]
splitL SZ = id
splitL (SS n) = splitList >>> splitL n *** splitL n

merge :: forall (n :: INat). SINat n -> Tree n [Complex Float] -> [Complex Float]
merge SZ  = id {- same: just concatenate all using a Prim -}
merge (SS n) = merge n *** merge n >>> concatenate

fmapTIx :: SINat n -> ((Int, [Complex Float]) -> [Complex Float]) -> Int -> Tree n [Complex Float] -> Tree n [Complex Float]
fmapTIx SZ f k = (\v -> f (k, v))
fmapTIx (SS x) f k = fmapTIx x f k *** fmapTIx x f (k + (2 ^ (fromINat x :: Integer)))

swap :: ((a,b), (c, d)) -> ((a, c), (b, d))
swap = ((fst >>> fst) &&& (snd >>> fst)) &&&
       ((fst >>> snd) &&& (snd >>> snd))

zwT :: SINat n -> (([Complex Float],[Complex Float]) -> [Complex Float])
    -> (Tree n [Complex Float], Tree n [Complex Float]) -> Tree n [Complex Float]
zwT SZ f = f
zwT (SS x) f = swap >>> zwT x f *** zwT x f

fft :: SINat n -> Tree n [Complex Float] -> Tree n [Complex Float]
fft SZ = baseFFT
fft (SS x)
  = fft x {-EVENS-} *** fft x {-ODDS-} >>>
    id *** fmapTIx x (uncurry $ mulExp p2sx) 0 {- Multiply by exponential -} >>>
    zwT x addc {- Left side -} &&& zwT x subc {- right side -}
  where
    p2sx :: Integer
    p2sx = 2 ^ (fromINat (SS x) :: Integer)

fastFourierR :: forall (n :: INat). SINat n -> [Complex Float] -> [Complex Float]
fastFourierR cores = addPadding cores >>> splitL cores >>> fft cores >>> merge cores

---------------------------------------------------------------------------------
-- UTILITY FUNCTIONS

fastFourier :: forall n. (KnownNat n, IsSing (FromNat n))
            => [Complex Float] -> [Complex Float]
fastFourier = fastFourierR (sing :: SINat (FromNat n))

fft8core :: [Complex Float] -> [Complex Float]
fft8core = fastFourier @3
fft16core :: [Complex Float] -> [Complex Float]
fft16core = fastFourier @4

type family ToNat (i :: INat) :: Nat where
  ToNat 'Z = 0
  ToNat ('S n) = 1 + ToNat n

type family FromNat (i :: Nat) :: INat where
  FromNat 0 = 'Z
  FromNat n = 'S (FromNat (n-1))



fmapT :: SINat n -> (a -> b) -> Tree n a -> Tree n b
fmapT SZ f = f
fmapT (SS x) f = fmapT x f *** fmapT x f


data SDict a where
  SDict :: Show a => SDict a

getDict :: Show a => SINat n -> Proxy a -> SDict (Tree n a)
getDict SZ _ = SDict
getDict (SS n) p = case getDict n p of
                        SDict -> SDict

test :: SINat n -> Tree n Int -> String
test n = case getDict n (Proxy :: Proxy Int) of
           SDict -> show

test2 :: Show (Tree n Int) => SINat n -> Tree n Int -> Bool
test2 _ _ =  False

test3 :: SINat n -> Tree n Int -> Bool
test3 n t = case getDict n (Proxy :: Proxy Int) of
              SDict -> test2 n t
