{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.SessionTypes.Common
  ( Label (..)
  , Role (..)
  , RoleSet (..)
  , Alt (..)
  , adjust
  , addAlt
  , addLAlt
  , getAlt
  , emptyAlt
  , mapAlt
  , foldAlt
  ) where

import Data.Map ( Map )
import qualified Data.Map.Strict as Map
import Data.Text.Prettyprint.Doc ( Pretty, pretty )
import qualified Data.Text.Prettyprint.Doc as Pretty

newtype Label = Lbl { labelId  :: Integer }

instance Show Label where
  show = show . labelId

instance Eq Label where
  l1 == l2 = labelId l1 == labelId l2

instance Ord Label where
  l1 `compare` l2 = labelId l1 `compare` labelId l2

instance Pretty Label where
  pretty (labelId -> l) = Pretty.hcat [ pretty "l", pretty l ]

newtype Role   = Rol { roleName :: Integer }
  deriving (Eq, Ord)

instance Show Role where
  show = show . roleName

instance Pretty Role where
  pretty (roleName -> r) = Pretty.hcat [ pretty "r", pretty r ]

newtype RoleSet = RS { unRS :: [Role] }

instance Pretty RoleSet where
  pretty (RS [r]) = pretty r
  pretty rs =
    Pretty.braces $! Pretty.hsep $! Pretty.punctuate (pretty ',')
    $! map pretty $! unRS rs

newtype Alt c = Alt { altMap :: Map Label c }
  deriving Show

deriving instance Foldable Alt
deriving instance Functor Alt
deriving instance Traversable Alt

mapAlt :: (Label -> c -> d) -> Alt c -> Alt d
mapAlt f Alt { altMap = m } = Alt $! Map.mapWithKey f m

foldAlt :: (a -> b -> a) -> a -> Alt b -> a
foldAlt f z Alt { altMap = m } = Map.foldl' f z m

adjust :: Label -> (c -> Maybe c) -> Alt c -> Maybe (Alt c)
adjust l f = fmap Alt . madjust . altMap
  where
    madjust m
      | Just c <- Map.lookup l m, Just c' <- f c = Just $! Map.insert l c' m
      | otherwise                              = Nothing

addAlt :: Integer -> c -> Alt c -> Alt c
addAlt i = addLAlt (Lbl i)

addLAlt :: Label -> c -> Alt c -> Alt c
addLAlt i c = Alt . Map.insert i c . altMap


getAlt :: Integer -> Alt c -> Maybe c
getAlt i = Map.lookup (Lbl i) . altMap

emptyAlt :: Alt c
emptyAlt = Alt Map.empty

instance Pretty c => Pretty (Alt c) where
  pretty (Map.assocs . altMap -> [(_, c)]) = pretty c

  pretty (Map.assocs . altMap -> m) =
      Pretty.braces $ Pretty.align $ Pretty.vsep $
        Pretty.punctuate Pretty.semi $ map (uncurry prettyLblAlt) m
    where
      prettyLblAlt lbl c = Pretty.hsep [ pretty lbl, pretty ".", pretty c ]
