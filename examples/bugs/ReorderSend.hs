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
module ReorderSend where

import Control.CArr.CSyn
import Language.SPar.Skel ( printASkel, mempty )

buggy :: forall f. CAlg f => f Int Int
buggy = cfun $ \x -> (3 + x) + (1 + 2)
