{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
module FFT where

import Prelude ( Double, ($), Integer, Num(..), Int, (^), Bool(..), otherwise )
-- import qualified Prelude

import Data.C ( CVal )
import Data.Complex
import Control.CCat
import Control.CArr
import Control.CArr.CSyn ( SINat, Sing(..), Tree
                         , cdictTree, withCDict, toInteger, withSize )
import Language.SPar.Skel ( (:->), liftAlg )

tsplit' :: forall a n f. (CVal a, CArrLift (:->) f, CAlg f) => SINat n -> Int
        -> (Int -> Int -> a :-> (a, a))
        -> f a (Tree n a)
tsplit' SZ _acc _f = newProc
tsplit' (SS n) acc f =
  withCDict (cdictTree @a n) $ lift (f wl wr) >>>
  (tsplit' n wl f *** tsplit' n wr f)
  where
    wl = acc
    wr = acc + 2 ^ toInteger n

tsplit :: forall a n f. (CArrLift (:->) f, CAlg f, CVal a)
       => SINat n
       -> (Int -> Int -> a :-> (a, a))
       -> f a (Tree n a)
tsplit n = tsplit' n 0

tfold  :: forall f a n. (CAlg f, CVal a)
       => SINat n -> f (a, a) a -> f (Tree n a) a
tfold  SZ _f = id
tfold (SS n) f =
  withCDict (cdictTree @a n) $ (tfold n f *** tfold n f) >>> f

zipTree :: forall a b n f. (CVal a, CVal b, CAlg f, CArrLift (:->) f)
        => SINat n
        -> Prelude.Bool
        -> Int
        -> Int
        -> ((Int, Int), (a, a)) :-> b
        -> f (Tree n a, Tree n a) (Tree n b)
zipTree SZ b l w f
  | b = lift ((lit l &&& lit w) &&& id >>> f)
  | otherwise = (snd &&& fst) >>> lift ((lit l &&& lit w) &&& id >>> f)
zipTree (SS x) b l w f =
  withCDict (cdictTree @a x) $
  withCDict (cdictTree @b x) $
  let swap = ((fst >>> fst) &&& (snd >>> fst)) &&&
             ((fst >>> snd) &&& (snd >>> snd))
  in swap >>> (zipTree x b l w f *** zipTree x b l (w + 2 ^ toInteger x) f)

type D a = a

fmapT :: CAlg f
        => SINat n
        -> f (D [Complex Double]) (D [Complex Double])
        -> f (Tree n (D [Complex Double])) (Tree n (D [Complex Double]))
fmapT SZ f = f
fmapT (SS x) f
  = withCDict (cdictTree @(D [Complex Double]) x) $ fmapT x f *** fmapT x f


fmapTIx :: CAlg f
        => SINat n
        -> f (Int, D [Complex Double]) (D [Complex Double])
        -> Int
        -> f (Tree n (D [Complex Double])) (Tree n (D [Complex Double]))
fmapTIx SZ f k = lit k &&& id >>> f
fmapTIx (SS x) f k
  = withCDict (cdictTree @(D [Complex Double]) x) $
  fmapTIx x f k *** fmapTIx x f (k + (2 ^ (toInteger x :: Integer)))

-- addPadding :: CAlg f => f (D [Complex Double]) (D [Complex Double])
-- addPadding = prim "add_padding"

deinterleave :: Int -> Int -> (D [Complex Double]) :-> (D [Complex Double], D [Complex Double])
deinterleave wl wr = lit wl &&& lit wr >>> prim "deinterleave"

intlit :: (CAlg f, CVal a) => SINat n -> f a Int
intlit i = fromInteger $ toInteger i + 1

fftTree :: (CArrLift (:->) f, CAlg f)
        => SINat n
        -> Int
        -> f (Tree n (D [Complex Double])) (Tree n (D [Complex Double]))
fftTree SZ w
  = liftAlg (intlit SZ &&& (lit w &&& id) >>> prim "baseFFT")
fftTree (SS x) w
  = withCDict (cdictTree @(D [Complex Double]) x) $
    (fftTree x w {-EVENS-} *** fftTree x (w + 2^ toInteger x){-ODDS-}) >>>
    id *** fmapTIx x (lit ps2x &&& id >>> prim "map_exp") 0 {- Multiply left side by exponential -} >>>
    zipTree x True lvl w addc &&& zipTree x False lvl (w + 2^ toInteger x) subc
  where
    lvl :: Int
    lvl = fromInteger (toInteger (SS x) + 1)
    ps2x :: Int
    ps2x = 2 ^ toInteger (SS x)
    addc :: ((Int, Int), (D [Complex Double], D [Complex Double])) :-> (D [Complex Double])
    addc = prim "zip_add"
    subc :: ((Int, Int), (D [Complex Double], D [Complex Double])) :-> (D [Complex Double])
    subc = id *** (snd &&& fst) >>> prim "zip_sub"

fft :: (CArrLift (:->) f, CAlg f)
    => SINat n -> f (D [Complex Double]) (D [Complex Double])
fft n =
  withCDict (cdictTree @(D [Complex Double]) n) $
  -- addPadding >>>
  tsplit n deinterleave >>> fftTree n 0 >>> tfold n (runAt 0 >>> prim "cat")

fft0 :: (CArrLift (:->) f, CAlg f)
     => f (D [Complex Double]) (D [Complex Double])
fft0 = withSize 0 fft

fft1 :: (CArrLift (:->) f, CAlg f)
     => f (D [Complex Double]) (D [Complex Double])
fft1 = withSize 1 fft

fft2 :: (CArrLift (:->) f, CAlg f)
     => f (D [Complex Double]) (D [Complex Double])
fft2 = withSize 2 fft

fft3 :: (CArrLift (:->) f, CAlg f)
     => f (D [Complex Double]) (D [Complex Double])
fft3 = withSize 3 fft

fft4 :: (CArrLift (:->) f, CAlg f)
     => f (D [Complex Double]) (D [Complex Double])
fft4 = withSize 4 fft

fft5 :: (CArrLift (:->) f, CAlg f)
     => f (D [Complex Double]) (D [Complex Double])
fft5 = withSize 5 fft

fft6 :: (CArrLift (:->) f, CAlg f)
     => f (D [Complex Double]) (D [Complex Double])
fft6 = withSize 6 fft

fft7 :: (CArrLift (:->) f, CAlg f)
     => f (D [Complex Double]) (D [Complex Double])
fft7 = withSize 7 fft

fft8 :: (CArrLift (:->) f, CAlg f)
     => f (D [Complex Double]) (D [Complex Double])
fft8 = withSize 8 fft
--
-- fft8 :: CAlg f => f (D [Complex Double]) (D [Complex Double])
-- fft8 = withSize 8 fft
  --where
  --  p2sx :: Integer
  --  p2sx = 2 ^ (fromINat (SS x) :: Integer)

--fft :: CAlg f => SINat n
--    -> f (Tree n [Complex Double]) (Tree n [Complex Double])

-- Below fails for some reason!
--fft :: CAlg f => SINat n
--    -> f (Tree n [Complex Double]) (Tree n [Complex Double])
--fft SZ = cfun $ prim "baseFFT"
--fft (SS n) = cfun $ \x ->
--  vlet (withCDict (cdictTree @[Complex Double] n) $ app (fft n) $ fst x) $ \l ->
--  vlet (app (fft n) $ snd x) $ \r -> _
  -- vlet (par (fft n) $ fst x) $ \l -> _
--fft n = cfun $ \z ->
--  case n of
--    SZ -> prim "baseFFT" z
--    (SS x) ->
--      vlet (par (app $ fft x) $ fst z) $ \l ->
--      vlet (par (app $ fft x) $ snd z) $ \r ->
--      _
