{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Alg
  ( module Data.C
  , AAlg(..)
  , Alg (..)
  , closeAlg
  , closeFun
  , fbvs
  , CAp (..)
  , CVar (..)
  , (:->)(..)
  , Poly(..)
  , CArrCmp(..)
  , prim
  , pmap
  , encName
  , tinl
  , tinr
  , fun
  , fix
  , mif
  -- , eqInt
  , printAlg
  , printFun
  , apair
  , afst
  , asnd
  , eqAlg
  , eqFun
  , ordAlg
  , ordFun
  , ASt
  , emptyASt
  , compileAlg
  , declareFun
  , cret
  , Num(..)
  , Fractional(..)
  ) where

import Prelude hiding ( id, (.), const )
import Type.Reflection hiding ( Fun )

import Data.C
import Data.Ratio ( numerator, denominator )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import Control.CCat
import Control.CArr
import Control.Monad.CGen
import Control.Monad.Extra ( whenM )

import Debug.Trace
debug :: String -> CGen st ()
debug s = trace s $ pure ()

data T = K | I | P T T | S T T

data Poly k a b where
  PK :: CVal b => Poly 'K a b
  PI :: CVal a => Poly 'I a a
  PP :: (CVal b, CVal c) => Poly f a b -> Poly g a c -> Poly ('P f g) a (b, c)
  PS :: (CVal b, CVal c) => Poly f a b -> Poly g a c -> Poly ('S f g) a (Either b c)

type family PMap (k :: T) (a :: *) (t :: *) :: * where
  PMap 'K a t = t
  PMap 'I a _ = a
  PMap ('P f g) a (l, r) = (PMap f a l, PMap g a r)
  PMap ('S f g) a (Either l r) = Either (PMap f a l) (PMap g a r)

data CDict a where
  CDict :: CVal a => CDict a

cdict :: (CVal a, CVal b, CVal t) => f b -> Poly k a t -> CDict (PMap k b t)
cdict _ PK = CDict
cdict _ PI = CDict
cdict b (PP l r) = case (cdict b l, cdict b r) of
                     (CDict, CDict) -> CDict
cdict b (PS l r) = case (cdict b l, cdict b r) of
                     (CDict, CDict) -> CDict

pmap :: forall a b t k. (CVal t, CVal a, CVal b)
     => Poly k a t -> a :-> b -> (t :-> PMap k b t, Poly k b (PMap k b t))
pmap PK _f = (Fun $ Abs $ \x -> x, PK)
pmap PI  f = (Fun $ Abs $ \x -> Ap f x, PI)
pmap (PP l r) f =
  case (cdict (getCTy :: CTy b) l, cdict (getCTy :: CTy b) r) of
    (CDict, CDict) ->
      (Fun $ Abs $ \x -> Pair (Ap lf $ Fst x) (Ap rf $ Snd x), PP pl pr)
  where
    (lf, pl) = pmap l f
    (rf, pr) = pmap r f
pmap (PS l r) f =
  case (cdict (getCTy :: CTy b) l, cdict (getCTy :: CTy b) r) of
    (CDict, CDict) ->
      ( Fun $ Abs $ \x -> acase x
                          (fun $ \y -> Inl $ ap lf y)
                          (fun $ \y -> Inr $ ap rf y)
      , PS pl pr
      )
  where
    (lf, pl) = pmap l f
    (rf, pr) = pmap r f

data UnOp = Neg
  deriving (Eq, Ord)
data BinOp = Plus | Minus | Mult | Div | Mod
  deriving (Eq, Ord)
data CmpOp = Le | Lt | Ge | Gt | Eq
  deriving (Eq, Ord)

data Alg t where
  Lit  :: CVal t => !t -> Alg t
  Prim :: !String -> t -> Alg t
  BVar :: !Integer -> Alg t
  CVal :: !CExpr -> Alg t -- ^ Internal use only

  -- Bool funcs
  BIf  :: CVal a => !(Alg Bool) -> !(Alg a) -> !(Alg a) -> Alg a

  -- Num functions
  UnOp :: (Num a, CVal a)  => !UnOp  -> !(Alg a) -> Alg a
  BinOp :: (Num a, CVal a) => !BinOp -> !(Alg a) -> !(Alg a) -> Alg a
  CmpOp :: (Num a, CVal a) => !CmpOp -> !(Alg a) -> !(Alg a) -> Alg Bool

  -- First order
  Ap   :: (CVal a, CVal b) => !(a :-> b) -> !(Alg a) -> Alg b
  Abs  :: (CVal a, CVal b) => !(Alg a -> Alg b) -> Alg (a -> b)

  Fst  :: (CVal a, CVal b) => !(Alg (a, b)) -> Alg a
  Snd  :: (CVal a, CVal b) => !(Alg (a, b)) -> Alg b
  Pair :: (CVal a, CVal b) => !(Alg a) -> !(Alg b) -> Alg (a, b)

  Inl  :: (CVal a, CVal b) => !(Alg a) -> Alg (Either a b)
  Inr  :: (CVal a, CVal b) => !(Alg b) -> Alg (Either a b)
  Case :: (CVal a, CVal b, CVal c)
       => !(Alg (Either a b)) -> !(a :-> c) -> !(b :-> c) -> Alg c

  Vec  :: CVal a => !(Int :-> a) -> !(Alg Int) -> Alg [a]
  VLit :: CVal a => ![Alg a] -> Alg [a] -- Static initialization
  Proj :: CVal a => !(Alg Int) -> !(Alg [a]) -> Alg a
  VLen :: CVal a => !(Alg [a]) -> Alg Int
  VTake :: CVal a => !(Alg Int) -> !(Alg [a]) -> Alg [a]
  VDrop :: CVal a => !(Alg Int) -> !(Alg [a]) -> Alg [a]

  Bot  :: Alg t
  Fix  :: (CVal a, CVal b) => !(a :-> b -> a :-> b) -> Alg (a -> b)

closeFun :: forall a b c. (Typeable a, Typeable b, Typeable c)
         => Integer -> a :-> b -> Alg c -> a :-> b
closeFun i (Fun f) x = Fun (closeAlg i f x)

closeAlg :: forall a b. (Typeable a, Typeable b)
         => Integer -> Alg a -> Alg b -> Alg a
closeAlg l  e@(BVar  x    ) a =
  case eqTypeRep (typeRep :: TypeRep a) (typeRep :: TypeRep b) of
    Just HRefl ->
      if l Prelude.== x then a else e
    _ -> e
closeAlg l  (BIf   b x y) a =
  BIf (closeAlg l b a) (closeAlg l x a) (closeAlg l y a)
closeAlg l  (UnOp  o x  ) a =
  UnOp o (closeAlg l x a)
closeAlg l  (BinOp o x y) a =
  BinOp o (closeAlg l x a) (closeAlg l y a)
closeAlg l  (CmpOp o x y) a =
  CmpOp o (closeAlg l x a) (closeAlg l y a)
closeAlg l  (Ap    f x  ) a =
  Ap (Fun $ closeAlg l (unFun f) a) (closeAlg l x a)
closeAlg l  (Abs   x    ) a =
  Abs $ \v -> closeAlg l (x v) a
closeAlg l  (Fst   x    ) a =
  Fst $ closeAlg l x a
closeAlg l  (Snd   x    ) a =
  Snd $ closeAlg l x a
closeAlg l  (Pair  x y  ) a =
  Pair (closeAlg l x a) (closeAlg l y a)
closeAlg l  (Inl   x    ) a =
  Inl $ closeAlg l x a
closeAlg l  (Inr   x    ) a =
  Inr $ closeAlg l x a
closeAlg l  (Case  e x y) a =
  Case (closeAlg l e a) (Fun $ closeAlg l (unFun x) a) (Fun $ closeAlg l (unFun y) a)
closeAlg l  (Vec   i x  ) a =
  Vec (Fun $ closeAlg l (unFun i) a) (closeAlg l x a)
closeAlg l  (VLit  x    ) a =
  VLit $ map (flip (closeAlg l) a) x
closeAlg l  (Proj  i x  ) a =
  Proj (closeAlg l i a) (closeAlg l x a)
closeAlg l  (VLen  x    ) a =
  VLen (closeAlg l x a)
closeAlg l  (VTake i x  ) a =
  VTake (closeAlg l i a) (closeAlg l x a)
closeAlg l  (VDrop i x  ) a =
  VDrop (closeAlg l i a) (closeAlg l x a)
closeAlg _  e@Fix {} _ = e
closeAlg _  e@Lit {} _ = e
closeAlg _  e@Prim{} _ = e
closeAlg _  e@Bot {} _ = e
closeAlg _  e@CVal{} _ = e

fbvs :: Alg a -> Set Integer
fbvs (BVar  x    ) = Set.singleton x
fbvs (BIf   b x y) = fbvs b `Set.union` fbvs x `Set.union` fbvs y
fbvs (UnOp  _ x  ) = fbvs x
fbvs (BinOp _ x y) = fbvs x `Set.union` fbvs y
fbvs (CmpOp _ x y) = fbvs x `Set.union` fbvs y
fbvs (Ap    f x  ) = fbvs (unFun f) `Set.union` fbvs x
fbvs (Abs   x    ) = fbvs (x Bot)
fbvs (Fst   x    ) = fbvs x
fbvs (Snd   x    ) = fbvs x
fbvs (Pair  x y  ) = fbvs x `Set.union` fbvs y
fbvs (Inl   x    ) = fbvs x
fbvs (Inr   x    ) = fbvs x
fbvs (Case  e x y) = fbvs e `Set.union` fbvs (unFun x) `Set.union` fbvs (unFun y)
fbvs (Vec   i x  ) = fbvs (unFun i) `Set.union` fbvs x
fbvs (VLit  x    ) = Set.unions $ map fbvs x
fbvs (Proj  i x  ) = fbvs i `Set.union` fbvs x
fbvs (VLen  x    ) = fbvs x
fbvs (VTake i x  ) = fbvs i `Set.union` fbvs x
fbvs (VDrop i x  ) = fbvs i `Set.union` fbvs x
fbvs (Fix f)      = fbvs (unFun (f (Fun Bot))) -- FIXME: recursive functions must be closed!
fbvs Lit {}      = Set.empty
fbvs Prim{}      = Set.empty
fbvs Bot {}      = Set.empty
fbvs CVal{}      = Set.empty

expensive :: Alg t -> Bool
expensive Prim{} = True
expensive VTake{} = True
expensive VDrop{} = True
expensive Fix{} = True
expensive (Ap f x) = expensive (unFun f) || expensive x
expensive (Abs f) = expensive $ f (BVar 0)
expensive (Pair l r) = expensive l || expensive r
expensive (Inl l) = expensive l
expensive (Inr l) = expensive l
expensive _ = False

(|<) :: (Num a, CVal a) => Alg a -> Alg a -> Alg Bool
(|<) = CmpOp Lt
(|<=) :: (Num a, CVal a) => Alg a -> Alg a -> Alg Bool
(|<=) = CmpOp Le
(|>) :: (Num a, CVal a) => Alg a -> Alg a -> Alg Bool
(|>) = CmpOp Gt
(|>=) :: (Num a, CVal a) => Alg a -> Alg a -> Alg Bool
(|>=) = CmpOp Ge
(|==) :: (Num a, CVal a) => Alg a -> Alg a -> Alg Bool
(|==) = CmpOp Eq

neg :: (Num a, CVal a) => Alg a -> Alg a
neg = UnOp Neg

instance (CVal a, Num a) => Num (Alg a) where
  (+) = BinOp Plus
  (*) = BinOp Mult
  (-) = BinOp Minus
  abs v = BIf (v |< Lit 0) (neg v) v
  signum v = BIf (v |< Lit 0) (Lit $ -1) (Lit $ 1)
  fromInteger i = Lit $ fromInteger i
  negate = neg

instance (CVal a, Num a) => Fractional (Alg a) where
  (/) = BinOp Div
  recip x = 1 / x
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

newtype (:->) a b = Fun { unFun :: Alg (a -> b)}


mif :: CVal a => (Bool, a) :-> Either a a
mif = fun $ \i -> BIf (afst i) (Inl $ asnd i) (Inr $ asnd i)

ordAlg :: Integer -> Alg a -> Alg b -> Ordering
ordAlg _ (Lit (x :: a)) (Lit (y :: b)) =
  case eqTypeRep tx ty of
    Just HRefl -> compare x y
    Nothing -> compare (SomeTypeRep tx) (SomeTypeRep ty)
  where
    tx = typeRep :: TypeRep a
    ty = typeRep :: TypeRep b
ordAlg _ (Lit _) _ = LT
ordAlg _ _ (Lit _) = GT
ordAlg _ (Prim f _) (Prim g _) = compare f g
ordAlg _ (Prim _ _) _ = LT
ordAlg _ _ (Prim _ _) = GT
ordAlg _ (BVar x) (BVar y) = compare x y
ordAlg _ (BVar _) _ = LT
ordAlg _ _ (BVar _) = GT
ordAlg _ (CVal _) (CVal _) = error "Panic! Cannot compare C Expressions"
ordAlg _ (CVal _) _ = LT
ordAlg _ _ (CVal _) = GT
ordAlg l (BIf b1 l1 r1) (BIf b2 l2 r2) =
  case (ordAlg l b1 b2, ordAlg l l1 l2, ordAlg l r1 r2) of
    (EQ, EQ, o) -> o
    (EQ, o , _) -> o
    (o , _ , _) -> o
ordAlg l (UnOp o1 x1) (UnOp o2 x2) =
  case (compare o1 o2, ordAlg l x1 x2) of
    (EQ, o ) -> o
    (o , _ ) -> o
ordAlg _ UnOp{} _ = LT
ordAlg _ _ UnOp{} = GT
ordAlg l (BinOp b1 l1 r1) (BinOp b2 l2 r2) =
  case (compare b1 b2, ordAlg l l1 l2, ordAlg l r1 r2) of
    (EQ, EQ, o) -> o
    (EQ, o , _) -> o
    (o , _ , _) -> o
ordAlg _ BinOp{} _ = LT
ordAlg _ _ BinOp{} = GT
ordAlg l (CmpOp b1 l1 r1) (CmpOp b2 l2 r2) =
  case (compare b1 b2, ordAlg l l1 l2, ordAlg l r1 r2) of
    (EQ, EQ, o) -> o
    (EQ, o , _) -> o
    (o , _ , _) -> o
ordAlg _ CmpOp{} _ = LT
ordAlg _ _ CmpOp{} = GT
ordAlg _ BIf{} _ = LT
ordAlg _ _ BIf{} = GT
ordAlg l (Ap f x) (Ap g y) =
  case ordFun l f g of
    EQ -> ordAlg l x y
    o  -> o
ordAlg _ Ap{} _ = LT
ordAlg _ _ Ap{} = GT
ordAlg l (Abs f) (Abs g) = ordAlg (l+1) (f (BVar l)) (g (BVar l))
ordAlg _ (Abs _) _ = LT
ordAlg _ _ (Abs _) = GT
ordAlg l (Fst x) (Fst y) = ordAlg l x y
ordAlg _ (Fst _) _ = LT
ordAlg _ _ (Fst _) = GT
ordAlg l (Snd x) (Snd y) = ordAlg l x y
ordAlg _ (Snd _) _ = LT
ordAlg _ _ (Snd _) = GT
ordAlg l (Inl x) (Inl y) = ordAlg l x y
ordAlg _ (Inl _) _ = LT
ordAlg _ _ (Inl _) = GT
ordAlg l (Inr x) (Inr y) = ordAlg l x y
ordAlg _ (Inr _) _ = LT
ordAlg _ _ (Inr _) = GT
ordAlg l (Case v x y) (Case w z t) =
  case (ordAlg l v w, ordFun l x z, ordFun l y t) of
    (EQ, EQ, o) -> o
    (EQ, o , _) -> o
    (o , _ , _) -> o
ordAlg _ Case{} _ = LT
ordAlg _ _ Case{} = GT
ordAlg l (Pair x y) (Pair z t) =
  case (ordAlg l x z, ordAlg l y t) of
    (EQ, o) -> o
    (o , _) -> o
ordAlg _ Pair{} _ = LT
ordAlg _ _ Pair{} = GT
ordAlg l (Vec x y) (Vec z t) =
  case (ordFun l x z, ordAlg l y t) of
    (EQ, o) -> o
    (o , _) -> o
ordAlg _ Vec{} _ = LT
ordAlg _ _ Vec{} = GT
ordAlg l (VLit x) (VLit y) =
  ordL x y
  where
    ordL (hx:tx) (hy:ty) =
      case ordAlg l hx hy of
        EQ -> ordL tx ty
        o  -> o
    ordL [] [] = EQ
    ordL [] _ = LT
    ordL _ [] = GT
ordAlg _ VLit{} _ = LT
ordAlg _ _ VLit{} = GT
ordAlg l (Proj i x) (Proj j y) =
  case (ordAlg l i j, ordAlg l x y) of
    (EQ, o) -> o
    (o , _) -> o
ordAlg _ Proj{} _ = LT
ordAlg _ _ Proj{} = GT
ordAlg l (VLen x) (VLen y) = ordAlg l x y
ordAlg _ VLen{} _ = LT
ordAlg _ _ VLen{} = GT
ordAlg l (VTake i x) (VTake j y) =
  case (ordAlg l i j, ordAlg l x y) of
    (EQ, o) -> o
    (o , _) -> o
ordAlg _ VTake{} _ = LT
ordAlg _ _ VTake{} = GT
ordAlg l (VDrop i x) (VDrop j y) =
  case (ordAlg l i j, ordAlg l x y) of
    (EQ, o) -> o
    (o , _) -> o
ordAlg _ VDrop{} _ = LT
ordAlg _ _ VDrop{} = GT
ordAlg _ Bot Bot = EQ
ordAlg _ Bot{} _ = LT
ordAlg _ _ Bot{} = GT
ordAlg l (Fix f) (Fix g) = ordFun (l+1) (f (Fun $ BVar l)) (g (Fun $ BVar l))

ordFun :: Integer -> a :-> b -> c :-> d -> Ordering
ordFun l (Fun a) (Fun b) = ordAlg l a b

eqAlg :: Integer -> Alg a -> Alg b -> Bool
eqAlg l a b = ordAlg l a b Prelude.== EQ

eqFun :: Integer -> a :-> b -> c :-> d -> Bool
eqFun l (Fun a) (Fun b) = eqAlg l a b

printAlg :: Integer -> Alg t -> String
printAlg _ (Lit x) = show x
printAlg _ (Prim s _) = s
printAlg _ (BVar i) = "?" ++ show i
printAlg _ (CVal i) = show i
printAlg l (BIf b x y) = "if (" ++ printAlg l b ++ ")\n  then ("
                         ++ printAlg l x ++ ")\n  else (" ++ printAlg l y ++ ")"
printAlg l (Ap (Fun f) x) = printAlg l f ++ "(" ++ printAlg l x ++ ")"
printAlg l (Abs f) = "(fun->" ++ printAlg (l+1) (f (BVar l)) ++ ")"
printAlg l (Fst x) = "fst (" ++ printAlg l x ++")"
printAlg l (Snd x) = "snd (" ++ printAlg l x ++")"
printAlg l (Pair x y) = "(" ++ printAlg l x ++"," ++ printAlg l y ++ ")"
printAlg l (Inl x) = "inl (" ++ printAlg l x ++")"
printAlg l (Inr x) = "inr (" ++ printAlg l x ++")"
printAlg l (Case v x y) = "case (" ++ printAlg l v ++")\n  " ++
                          " (" ++ printAlg (l+1) (ap x (BVar l)) ++ ")\n  " ++
                          " (" ++ printAlg (l+1) (ap y (BVar l)) ++ ")"
printAlg l (Vec f i) = "vec (?" ++ show l ++ "= 0 to " ++ printAlg l i ++")" ++
                       " {" ++ printAlg (l+1) (ap f (BVar l)) ++ "}"
printAlg l (VLit ls) = "[" ++ app (map (printAlg l) ls) ++ "]"
  where
    app [] = ""
    app [h] = h
    app (h:t) = h ++ "," ++ app t
printAlg l (VLen x) = "length (" ++ printAlg l x ++ ")"
printAlg l (Proj i v) = "(" ++ printAlg l v ++ ")!" ++
                        "(" ++ printAlg l i ++ ")"
printAlg _ Bot = "undef"
printAlg l (Fix f) = "fix ?" ++ show l ++ "\n" ++
                     "{" ++ printFun (l+1) (f (Fun $ BVar l)) ++ "\n}"
printAlg l (UnOp o e) = printOp o
  where
    printOp Neg = "- (" ++ se ++ ")"
    se = printAlg l e
printAlg l (BinOp o e1 e2) = "(" ++ se1 ++ ") " ++ printOp o ++ " (" ++ se2 ++ ")"
  where
    printOp Plus =  "+"
    printOp Minus =  "-"
    printOp Mult =  "*"
    printOp Div =  "/"
    printOp Mod =  "%"
    se1 = printAlg l e1
    se2 = printAlg l e2
printAlg l (CmpOp o e1 e2) = "(" ++ se1 ++ ") " ++ printOp o ++ " (" ++ se2 ++ ")"
  where
    printOp Le =  "<="
    printOp Lt =  "<"
    printOp Gt =  ">"
    printOp Ge =  ">="
    printOp Eq =  "=="
    se1 = printAlg l e1
    se2 = printAlg l e2
printAlg l (VTake o e) = "take (" ++ printAlg l o ++ ") (" ++ printAlg l e ++ ")"
printAlg l (VDrop o e) = "drop (" ++ printAlg l o ++ ") (" ++ printAlg l e ++ ")"

printFun :: Integer -> a :-> b -> String
printFun l (Fun ff) = printAlg l ff

fun :: (CVal a, CVal b) => (Alg a -> Alg b) -> a :-> b
fun f = Fun $ Abs f

aap :: (CVal a, CVal b) => a :-> b -> Alg a -> Alg b
aap f (Pair l r)
  | expensive l = aap (Fun $ Abs $ \v -> aap f (Pair v r)) l
  | expensive r = aap (Fun $ Abs $ \v -> aap f (Pair l v)) r
aap (Fun (Abs f)) v
  | not (expensive v) = f v
aap f v = Ap f v

afst :: (CVal a, CVal b) => Alg (a, b) -> Alg a
afst (Pair l _) = l
afst t = Fst t

asnd :: (CVal a, CVal b) => Alg (a, b) -> Alg b
asnd (Pair _ r) = r
asnd t = Snd t

apair :: (CVal a, CVal b) => Alg a -> Alg b -> Alg (a, b)
apair (Fst (BVar e1)) (Snd (BVar e2))
  | e1 Prelude.== e2 = BVar $ e1
apair (Fst (CVal e1@(CVar v1 _))) (Snd (CVal (CVar v2 _)))
  | v1 Prelude.== v2 = CVal e1
apair e1 e2 = Pair e1 e2

acase :: (CVal a, CVal b, CVal c) => Alg (Either a b) -> a :-> c -> b :-> c -> Alg c
acase (Inl v) f _ = ap f v
acase (Inr v) _ g = ap g v
acase (Case v l r) f g = acase v (fun $ \x -> acase (ap l x) f g) (fun $ \x -> acase (ap r x) f g)
acase (BIf b l r) f g = BIf b (acase l f g) (acase r f g)
acase v f g = Case v f g

tinl :: (CVal a, CVal b) => Alg a -> t b -> Alg (Either a b)
tinl v _ = Inl v

tinr :: (CVal a, CVal b) => t a -> Alg  b -> Alg (Either a b)
tinr _ v = Inr v

encName :: a :-> b -> String
encName (Fun x) = encAlg x

-- Only returns sensible names if
encAlg :: Alg a -> String
encAlg (Prim f _) = f
encAlg (Ap f x) = encName f ++ encAlg x
encAlg (Abs v) = encAlg $ v $ BVar 0
encAlg BinOp{} = "bin"
encAlg UnOp{} = "un"
encAlg CmpOp{} = "cmp"
encAlg (Fst _) = "proj_1"
encAlg (Snd _) = "proj_2"
encAlg (BVar _) = "var"
encAlg (CVal _) = "evar"
encAlg BIf{} = "bif"
encAlg (Lit l) = "lit_" ++ show l
encAlg (Case _ f g) = "case_" ++ encName f ++ "_" ++ encName g
encAlg (Proj _ _) = "prj"
encAlg (Pair _ _) = "mkpair"
encAlg (Inl _) = "mkeither"
encAlg (Inr _) = "mkeither"
encAlg (Vec _ _) = "mkvec"
encAlg (VLit _) = "mkvec"
encAlg (VLen _) = "get_size"
encAlg (VTake i _) = "take_" ++ encAlg i
encAlg (VDrop i _) = "drop_" ++ encAlg i
encAlg Bot = "error"
encAlg (Fix f) = "fix_" ++ encName (f (Fun $ BVar 0))

instance CArrCnst (:->) Alg where
  const e = Fun $ Abs $ \_ -> e

instance CCat (:->) where
  id = Fun $ Abs (\v -> v)
  f . g = Fun $ Abs (\v -> ap f (ap g v))

instance CArr (:->) where
  arr n v = Fun $ Prim n v
  lit l = const $ Lit l
  fst = Fun $ Abs (\v -> afst v)
  snd = Fun $ Abs (\v -> asnd v)
  first f = Fun $ Abs (\v -> apair (ap f (afst v)) (asnd v))
  second f = Fun $ Abs (\v -> apair (afst v) (ap f (asnd v)))
  f &&& g = Fun $ Abs (\v -> apair (ap f v) (ap g v))
  f *** g = Fun $ Abs (\v -> apair (ap f (afst v)) (ap g (asnd v)))

instance CArrChoice (:->) where
  inl = fun Inl
  inr = fun Inr
  left f  = fun (\v -> acase v (inl . f) inr)
  right f = fun (\v -> acase v inl (inr . f))
  f +++ g = fun (\v -> acase v (inl . f) (inr . g))
  f ||| g = fun (\v -> acase v f g)
  distrL = fun (\v -> acase (afst v) (inl . (id &&& (const (asnd v))))
                 (inr . (id &&& (const (asnd v)))))

instance CArrIf (:->) where
  ifThenElse test l r = test &&& id >>> mif >>> l ||| r

instance (CVal a, CVal b, Num b) => Num (a :-> b) where
  f + g = (f &&& g) >>> (fun $ \v -> afst v + asnd v)
  f * g = (f &&& g) >>> (fun $ \v -> afst v * asnd v)
  abs f = f >>> (fun $ \v -> abs v)
  signum f = f >>> (fun $ \v -> signum v)
  negate f = f >>> (fun $ \v -> negate v)
  fromInteger i = fun $ \_ -> fromInteger i

instance (CVal a, CVal b, Num b) => Fractional (a :-> b) where
  f / g = (f &&& g) >>> (fun $ \v -> afst v / asnd v)
  recip x = 1 / x
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

instance CArrCmp (:->) where
  f < g = f &&& g >>> (fun $ \v -> afst v |< asnd v)
  f <= g = f &&& g >>> (fun $ \v -> afst v |<= asnd v)
  f >= g = f &&& g >>> (fun $ \v -> afst v |>= asnd v)
  f > g = f &&& g >>> (fun $ \v -> afst v |> asnd v)
  f == g = f &&& g >>> (fun $ \v -> afst v |== asnd v)

instance CArrVec (:->) where
  proj = Fun $ Abs $ \v -> Proj (afst v) (asnd v)
  vec f = Fun $ Abs (\v -> Vec (fun $ \x -> ap f (apair x (asnd v))) (afst v))
  vsize = Fun $ Abs $ \v -> VLen v
  vtake = Fun $ Abs $ \v -> VTake (afst v) (asnd v)
  vdrop = Fun $ Abs $ \v -> VDrop (afst v) (asnd v)

instance CArrFix (:->) where
  fix f = Fun $ Fix f
  kfix _n f = fix f
    --  n Prelude.<= 0 = fix f
    --  otherwise = f (kfix (n-1) f)

instance CArrPar (:->) where
  newProc = id
  runAt _ = id

instance CVar Alg where
  var = BVar 0

instance CAp (:->) Alg where
  ap = aap

cSeq :: [CBlockItem] -> CStat
cSeq l = CCompound [] l undefNode

cret :: CExpr -> CExpr -> [CBlockItem]
cret rv e = [CBlockStmt $ cExpr $ CAssign CAssignOp rv e undefNode]

data AAlg where
  AAlg :: (CVal a, CVal b) => a :-> b -> AAlg

instance Eq AAlg where
  AAlg f == AAlg g = eqFun 0 f g

instance Ord AAlg where
  compare (AAlg f) (AAlg g) = ordFun 0 f g


type ASt = Map AAlg CExpr

emptyASt :: ASt
emptyASt = Map.empty

compileAlg :: CVal a => Alg a -> CGen ASt (CExpr, [CBlockItem])
compileAlg e
  | getTy e Prelude.== ECUnit = pure (cVar cUnit, [])
compileAlg (Lit l) = (,[]) <$> cVal l
compileAlg (Prim v _) = pure (cVar $ internalIdent v, [])
compileAlg (BVar i) = error $ "Panic! Cannot find open term! " ++ show i
compileAlg (CVal v) = pure (v, [])
compileAlg (BIf b x y) = do
  (rv, dv) <- declVar x
  (v, cb) <- compileAlg b
  (rv1, cx) <- compileAlg x
  (rv2, cy) <- compileAlg y
  pure (rv, dv ++ cb ++
    [CBlockStmt $ CIf v (cSeq $ cx ++ cret rv rv1)
      (Just $ cSeq $ cy ++ cret rv rv2) undefNode])
compileAlg (UnOp o x) = do
  (rv, cx) <- compileAlg x
  pure (CUnary (go o) rv undefNode, cx)
  where
    go Neg = CMinOp
compileAlg (BinOp o x y) = do
  (v, dv) <- declVar x
  (rv1, cx) <- compileAlg x
  (rv2, cy) <- compileAlg y
  pure (v, dv ++ cx ++ cy ++
    [CBlockStmt $ cExpr $ cAssign v $ CBinary (go o) rv1 rv2 undefNode])
  where
    go Plus = CAddOp
    go Minus = CSubOp
    go Mult = CMulOp
    go Div = CDivOp
    go Mod = CRmdOp
compileAlg e@(CmpOp o x y) = do
  (v, dv) <- declVar e
  (vx, cx) <- compileAlg x
  (vy, cy) <- compileAlg y
  pure (v, dv ++ cx ++ cy ++
    [CBlockStmt $ cExpr $ cAssign v (CBinary (go o) vx vy undefNode)])
  where
    go Lt = CLeOp
    go Le = CLeqOp
    go Gt = CGrOp
    go Ge = CGeqOp
    go Eq = CEqOp
compileAlg (Ap f x) = do
  (v, cx) <- compileAlg x -- XXX: Fix strict semantics!!!!!
  (rv, cf) <- compileFun (unFun f) v
  pure (rv, cx ++ cf)
compileAlg t@(Fst e) = do
  (rv, dv) <- declVar t
  (v, cs) <- compileAlg e
  pure (rv, dv ++ cs ++ cret rv (cMember v fstFld))
compileAlg t@(Snd e) = do
  (rv, dv) <- declVar t
  (v, cs) <- compileAlg e
  pure (rv, dv ++ cs ++ cret rv (cMember v sndFld))
compileAlg e@(Pair e1 e2) = do
  (rv, dv) <- declVar e
  (v1, cs1) <- compileAlg e1
  (v2, cs2) <- compileAlg e2
  pure (rv, dv ++ cs1 ++ cret (cMember rv fstFld) v1 ++
         cs2 ++ cret (cMember rv sndFld) v2)
compileAlg e@(Inl _)
  | getTy e Prelude.== ECEither ECUnit ECUnit = pure (cVar cTagl, [])
compileAlg e@(Inl e1) = do
  (rv, dv) <- declVar e
  let c1 = CBlockStmt $ cExpr $ tL rv
  (ve, cs1) <- compileAlg e1
  pure (rv, dv ++ c1 : cs1 ++ cret (cMember (cMember rv valFld) inlFld) ve)
  where
    tL rv = CAssign CAssignOp ((cMember rv tagFld)) (cVar cTagl) undefNode
compileAlg e1@Inr{}
  | getTy e1 Prelude.== ECEither ECUnit ECUnit = pure (cVar cTagr, [])
compileAlg e@(Inr e1) = do
  (rv, dv) <- declVar e
  let c1 = CBlockStmt $ cExpr $ tR rv
  (ve, cs1) <- compileAlg e1
  pure (rv, dv ++ c1 : cs1 ++ cret (cMember (cMember rv valFld) inrFld) ve)
  where
    tR rv = CAssign CAssignOp (cMember rv tagFld) (cVar cTagr) undefNode
compileAlg ce@(Case e l r)
  | getTy e Prelude.== ECEither ECUnit ECUnit = do
  (rv, dv) <- declVar ce
  (v, s1) <- compileAlg e
  (rv1, sl) <- compileFun (unFun l) (cVar cUnit)
  (rv2, sr) <- compileFun (unFun r) (cVar cUnit)
  pure (rv, dv ++ s1 ++ [cs v (sl ++ cret rv rv1) (sr ++ cret rv rv2)])
  where
    cs v sl sr = CBlockStmt $ cCase v sl sr
compileAlg ce@(Case e l r) = do
  (rv, dv) <- declVar ce

  (v, s1) <- compileAlg e
  (rv1, sl) <- compileFun (unFun l) (cMember (cMember v valFld) inlFld)
  (rv2, sr) <- compileFun (unFun r) (cMember (cMember v valFld) inrFld)

  pure (rv, dv ++ s1 ++
         [CBlockStmt $ cCase (cMember v tagFld)
           (sl ++ cret rv rv1) (sr ++ cret rv rv2)])
compileAlg Vec{} = error "FIXME: vectors not yet supported"
compileAlg VLit{} = error "FIXME: vectors not yet supported"
compileAlg Proj{} = error "FIXME: vectors not yet supported"
compileAlg (VLen x) = do
  (v, s1) <- compileAlg x
  pure (cMember v sizeFld, s1)
compileAlg (VTake i x) = do
  (v, dv) <- declVar x
  (vi, s1) <- compileAlg i
  (vx, s2) <- compileAlg x
  pure (v, dv ++ s1 ++ s2 ++
         [ CBlockStmt $ cExpr $ cAssign (cMember v sizeFld) vi
         , CBlockStmt $ cExpr $ cAssign (cMember v elemsFld) (cMember vx elemsFld)
         ])
compileAlg (VDrop i x) = do
  (v, dv) <- declVar x
  (vi, s1) <- compileAlg i
  (vx, s2) <- compileAlg x
  pure (v, dv ++ s1 ++ s2 ++
    [ CBlockStmt $ cExpr $ cAssign (size v) (cMinus vx vi)
    , CBlockStmt $ cExpr $ cAssign (elems v) (cPlus vx vi)])
  where
    size rv = cMember rv sizeFld
    elems rv = cMember rv elemsFld
    cPlus rv v = CBinary CAddOp (elems rv) v undefNode
    cMinus rv v = CBinary CSubOp (size rv) v undefNode
compileAlg Bot{} = pure ((CConst (CIntConst (cInteger $ -1) undefNode)),
                         errorAndExit)
--compileAlg Fix{} _ = error "Panic! A recursive function cannot be a CVal!"

domTy :: (CVal a, CVal b) => a :-> b -> CTy a
domTy _ = getCTy

codTy :: (CVal a, CVal b) => a :-> b -> CTy b
codTy _ = getCTy

compileFun :: (CVal a, CVal b)
           => Alg (a -> b)
           -> CExpr
           -> CGen ASt (CExpr, [CBlockItem])
compileFun (Abs f) x = compileAlg (f $ CVal x)
compileFun e@(Prim f _) x = do
  (rv, d) <- declVar (Ap (Fun e) (CVal x))
  whenM (not <$> isDeclared (internalIdent f)) $ do
    dt <- cTySpec $ domTy $ Fun e
    ct <- cTySpec $ codTy $ Fun e
    newHeaderFun (internalIdent f) ct [dt]
  pure $ (rv, d ++ cret rv fx)
  where
    fx = CCall (cVar $ internalIdent f) [x] undefNode
compileFun (BVar _) _  = error "Panic! Open term"
compileFun e@(CVal v) x  = do
  (rv, d) <- declVar (Ap (Fun e) (CVal x))
  pure (rv, d ++ cret rv (CCall v [x] undefNode)) -- Recursive calls
compileFun Bot _ = pure ((CConst (CIntConst (cInteger $ -1) undefNode)),
                         errorAndExit)
compileFun e@(Fix f) x = do
  me <- getUstate (Map.lookup (AAlg $ Fun e))
  fn <- case me of
          Just v -> pure v
          Nothing -> do
            fn <- freshN "fn"
            arg <- freshVar
            let ycty = codTy (f $ Fun Bot)
                xcty = domTy (f $ Fun Bot)
            xty <- cTySpec xcty
            yty <- cTySpec ycty
            (rv, fb) <- compileFun (unFun $ f (Fun $ CVal $ cVar fn)) (cVar arg)
            newFun (fn, yty) [(arg, xty)]
              (fb ++ [CBlockStmt $ CReturn (Just rv) undefNode])
            ustate $ Map.insert (AAlg $ Fun e) (cVar fn)
            pure $ cVar fn

  (rv, d) <- declVar (Ap (Fun e) (CVal x))
  pure (rv, d ++ cret rv (CCall fn [x] undefNode))

declareFun :: (CVal a, CVal b) => String -> a :-> b -> CGen ASt ()
declareFun fm f@(Fun (Prim fn _))
  | fm Prelude.== fn = whenM (not <$> isDeclared ifn) $ do
      debug $ "declaring " ++ fm
      let ycty = codTy f
          xcty = domTy f
      xty <- cTySpec xcty
      yty <- cTySpec ycty
      newHeaderFun ifn yty [xty]
      ustate $ Map.insert (AAlg f) (cVar ifn)
  where
    ifn = internalIdent fn
declareFun (internalIdent -> fn) (Fun (Fix f))
  = whenM (not <$> isDeclared fn) $ do
  arg <- freshVar
  let ycty = codTy (f $ Fun Bot)
      xcty = domTy (f $ Fun Bot)
  xty <- cTySpec xcty
  yty <- cTySpec ycty
  (rv, fb) <- compileFun (unFun $ f (Fun $ CVal $ cVar fn)) (cVar arg)
  newFun (fn, yty) [(arg, xty)]
    (fb ++ [CBlockStmt $ CReturn (Just rv) undefNode])

  ustate $ Map.insert (AAlg $ Fun (Fix f)) (cVar fn)
declareFun (internalIdent -> fn) f = whenM (not <$> isDeclared fn) $ do
  arg <- freshVar
  xty <- cTySpec xcty
  yty <- cTySpec ycty

  (rv, fb) <- compileFun (unFun f) (cVar arg)
  newFun (fn, yty) [(arg, xty)]
    (fb ++ [CBlockStmt $ CReturn (Just rv) undefNode])
  ustate $ Map.insert (AAlg $ f) (cVar fn)
  where
    ycty = codTy f
    xcty = domTy f



errorAndExit :: [CBlockItem]
errorAndExit =
  [ CBlockStmt $ CExpr (Just (CCall (CVar (internalIdent "printf") undefNode)
                              [CConst (CStrConst (cString "Error\n") undefNode)]
                              undefNode)) undefNode
  , CBlockStmt $ CExpr (Just (CCall (CVar (internalIdent "exit") undefNode)
                              [CUnary CMinOp (CConst (CIntConst (cInteger 1)
                                                      undefNode))
                              undefNode]
                             undefNode)) undefNode
  ]

instance CArrLift (:->) (:->) where
  lift f = f
