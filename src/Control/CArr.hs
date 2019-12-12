{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.CArr
  ( CArr (..)
  , prim
  , CVar (..)
  , CAp (..)
  , CArrCnst(..)
  , CArrChoice (..)
  , CArrIf (..)
  , CArrVec (..)
  , CArrCmp (..)
  , CArrFix (..)
  , CArrFrac
  , CArrPar (..)
  , CArrLift (..)
  , CAlg
  ) where

import qualified Prelude
import Prelude hiding (fst, snd, id)

import Data.C
import Control.CCat

infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||

class CVar v where
  var :: v a

class (CVar v, CArr f) => CAp f v where
  ap  :: (CVal a, CVal b) => f a b -> v a -> v b

class CAp t v => CArrCnst t v where
  const :: (CVal a, CVal b) => v b -> t a b

prim :: (CVal a, CVal b, CArr t) => String -> t a b
prim s = arr s undefined

class CCat t => CArr t where
  arr :: (CVal a, CVal b) => String -> (a -> b) -> t a b

  lit :: (CVal a, CVal b) => a -> t b a

  fst :: (CVal a, CVal b) => t (a, b) a
  fst = arr "fst" Prelude.fst

  snd :: (CVal a, CVal b) => t (a, b) b
  snd = arr "snd" Prelude.snd

  first :: (CVal a, CVal b, CVal c) => t a b -> t (a, c) (b, c)
  first = (*** id)

  second :: (CVal a, CVal b, CVal c) => t a b -> t (c, a) (c, b)
  second = (id ***)

  (***) :: (CVal a, CVal a', CVal b, CVal b')
    => t a a' -> t b b' -> t (a, b) (a', b')
  f *** g = first f >>> arr "swap" swap >>> first g >>> arr "swap" swap
    where swap ~(x,y) = (y,x)

  (&&&) :: (CVal a, CVal b, CVal c) => t a b -> t a c -> t a (b, c)
  f &&& g = arr "dup" (\b -> (b,b)) >>> f *** g

class (CArr f, CArr g) => CArrLift f g where
  lift :: (CVal a, CVal b) => f a b -> g a b

class CArr a => CArrChoice a where

  inl :: (CVal b, CVal c) => a b (Either b c)
  inl = arr "inl" Left

  inr :: (CVal b, CVal c) => a c (Either b c)
  inr = arr "inr" Right

  left :: (CVal b, CVal c, CVal d)
       => a b c -> a (Either b d) (Either c d)
  left = (+++ id)

  right :: (CVal b, CVal c, CVal d)
        => a b c -> a (Either d b) (Either d c)
  right = (id +++)

  (+++) :: (CVal b, CVal c, CVal b', CVal c')
        => a b c -> a b' c' -> a (Either b b') (Either c c')
  f +++ g = left f >>> arr "mirror" mirror >>> left g >>> arr "mirror" mirror
    where
      mirror :: Either x y -> Either y x
      mirror (Left x) = Right x
      mirror (Right y) = Left y

  (|||) :: (CVal b, CVal c, CVal d)
        => a b d -> a c d -> a (Either b c) d
  f ||| g = f +++ g >>> arr "untag" untag
    where
      untag (Left x) = x
      untag (Right y) = y

  distrL :: (CVal b, CVal c, CVal d)
         => a (Either b c, d) (Either (b, d) (c,d))

class CArrChoice t => CArrIf t where
  ifThenElse :: (CVal a, CVal b) => t a Bool -> t a b -> t a b -> t a b

-- No static size checking
class ((forall a. CVal a => Num (t a Int)), CArr t) =>
      CArrVec t where
  proj :: CVal a => t (Int, [a]) a
  vec :: (CVal a, CVal b) => t (Int, a) b -> t (Int, a) [b]
  vsize :: CVal a => t [a] Int

  vtake :: CVal a => t (Int, [a]) [a]
  vtake = vec proj
  vdrop :: CVal a => t (Int, [a]) [a]
  vdrop = vec (((fst + (snd >>> vsize)) &&& snd) >>> proj)

class CArr t => CArrFix t where
  fix :: (CVal a, CVal b) => (forall f. CAlg f => f a b -> f a b) -> t a b
  kfix :: (CVal a, CVal b) => Int -> (forall f. CAlg f => f a b -> f a b) -> t a b

class CArrCmp t where
  (<)  :: (Num b, CVal a, CVal b) => t a b -> t a b -> t a Bool
  (<=) :: (Num b, CVal a, CVal b) => t a b -> t a b -> t a Bool
  (>)  :: (Num b, CVal a, CVal b) => t a b -> t a b -> t a Bool
  (>=) :: (Num b, CVal a, CVal b) => t a b -> t a b -> t a Bool
  (==) :: (Num b, CVal a, CVal b) => t a b -> t a b -> t a Bool

class (forall a b. (Num b, CVal a, CVal b) => Fractional (t a b)) => CArrFrac t where
instance (forall a b. (Num b, CVal a, CVal b) => Fractional (t a b)) => CArrFrac t where

type PID = Integer

class CArr t => CArrPar t where
  newProc :: CVal a => t a a
  runAt :: CVal a => PID -> t a a

type CAlg t = (CArrIf t, CArrVec t, CArrCmp t, CArrFrac t, CArrPar t, CArrFix t)
