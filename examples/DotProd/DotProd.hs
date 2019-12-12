{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
module DotProd where

import Control.CArr.CSyn
import Language.SPar.Skel ( printASkel )

--timesBench :: CAlg f => f (TProd 32 [Int]) (TProd 32 [Int])
--timesBench = cfun $ pmap (psize @32) (prim "sum")

splitIn :: forall f n a. (CAlg f, CVal a)
        => SINat n
        -> f (Int, ([a],[a])) (Prod n ([a],[a]))
splitIn sz = cfun $ \x ->
  case sz of
    SZ -> snd x
    SS n ->
      withCDict (cdictProd @([a],[a]) n) $
      vlet (fst x) $ \s ->
      vlet (fst $ snd x) $ \l ->
      vlet (snd $ snd x) $ \r ->
      vlet (pair (vtake s l, vtake s r)) $ \t ->
      vlet (pair (vdrop s l, vdrop s r)) $ \d ->
      pair (t, app (splitIn n) (pair (s, d)))

dotProdN :: forall f n. CAlg f => SINat n -> f ([Double],[Double]) Double
dotProdN i = cfun $ \x ->
  vlet (vsize (fst x) - 1) $ \sz ->
  vlet (1 + sz / (1 + fromINat i)) $ \sz' ->
  if sz' == 0 then dot x
  else
    vlet (app (splitIn i) $ pair (sz', x)) $
       \z -> vlet (smap i (par dot) z) $
             \t -> pfold i (cfun $ \s -> fst s + snd s) t
  where
    dot :: Expr f ctx ([Double], [Double]) -> Expr f ctx Double
    dot = prim "dot"


dotProd1 :: CAlg f => f ([Double],[Double]) Double
dotProd1 = withSize 1 $ dotProdN

dotProd2 :: CAlg f => f ([Double],[Double]) Double
dotProd2 = withSize 2 $ dotProdN

dotProd4 :: CAlg f => f ([Double],[Double]) Double
dotProd4 = withSize 4 $ dotProdN

dotProd8 :: CAlg f => f ([Double],[Double]) Double
dotProd8 = withSize 8 $ dotProdN

dotProd16 :: CAlg f => f ([Double],[Double]) Double
dotProd16 = withSize 16 $ dotProdN

dotProd24 :: CAlg f => f ([Double],[Double]) Double
dotProd24 = withSize 24 $ dotProdN

dotProd32 :: CAlg f => f ([Double],[Double]) Double
dotProd32 = withSize 32 $ dotProdN

dotProd64 :: CAlg f => f ([Double],[Double]) Double
dotProd64 = withSize 64 $ dotProdN
