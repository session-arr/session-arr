module Control.CCat
  ( CCat(..)
  , (>>>)
  , (<<<)
  ) where

import Prelude hiding ( (.), const )

import Data.C

infixr 9 .
infixr 1 >>>, <<<

class CCat t where
  id  :: CVal v => t v v
  (.) :: (CVal a, CVal b, CVal c) => t b c -> t a b -> t a c

-- | Right-to-left composition
(<<<) :: (CCat cat, CVal a, CVal b, CVal c)
      => cat b c -> cat a b -> cat a c
(<<<) = (.)

-- | Left-to-right composition
(>>>) :: (CCat cat, CVal a, CVal b, CVal c)
      => cat a b -> cat b c -> cat a c
f >>> g = g . f
