{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module ScalarMulMat where

import Control.CArr.CSyn

scalarProd :: forall n f. (PAlg f, CValProd n [[Double]])
           => SINat n
           -> f (Prod n [[Double]]) (Prod n [[Double]])
scalarProd n =
  cfun $ smap @[[Double]] @[[Double]] n (par $ prim "prod")

catv :: forall n f a. (IsSing n, PAlg f, CVal a)
           => SINat n -> f (Prod n [a]) [a]
catv n =
  withCDict (cdictProd @[a] n) $ cfun $ sfold n (prim "cat" @@ 0)

parProd :: forall n f. (IsSing n, PAlg f)
        => SINat n -> f [[Double]] [[Double]]
parProd n = withCDict (cdictProd @[[Double]] n) $
  cfun $ \x -> catv n `app` (scalarProd n `app` (ssplitv @[Double] n `app` x))

parProd0    ,
  parProd1  ,
  parProd2  ,
  parProd3  ,
  parProd4  ,
  parProd8  ,
  parProd16 ,
  parProd32 ,
  parProd64 :: PAlg f => f [[Double]] [[Double]]
parProd0 = withSize 0 parProd
parProd1 = withSize 1 parProd
parProd2 = withSize 2 parProd
parProd3 = withSize 3 parProd
parProd4 = withSize 4 parProd
parProd8 = withSize 8 parProd
parProd16 = withSize 16 parProd
parProd32 = withSize 32 parProd
parProd64 = withSize 64 parProd
