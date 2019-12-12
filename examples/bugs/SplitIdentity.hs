{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RebindableSyntax #-}
module SplitIdentity where

import Language.SPar.Skel

spl :: PAlg f => f [Int] [Int]
spl = vtake 1

test :: PAlg f => f [Int] ([Int], [Int])
test = id &&& id
