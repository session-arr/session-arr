{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.C
  ( CVal (..)
  , CTy(..)
  , IsFun
  , eraseTy
  , getCTyR
  , getTy
  , eitherTy
  , pairTy
  , cTySpec
  , (<::)
  , tagFld
  , valFld
  , inlFld
  , inrFld
  , fstFld
  , sndFld
  , cCase
  , cTagl
  , cTagr
  , declVar
  , Int
  , Bool
  , Float
  , Double
  , String
  , Either
  ) where

import Control.Monad.CGen
import Type.Reflection ( Typeable, TypeRep, typeRep )
import Data.Complex


data CTy a where
  CUnit   :: CTy ()
  CInt    :: CTy Int
  CBool   :: CTy Bool
  CFlt    :: CTy Float
  CDbl    :: CTy Double
  CCplx   :: CTy (Complex Double)
  CStr    :: CTy String
  CPair   :: (CVal a, CVal b) => CTy a -> CTy b -> CTy (a,b)
  CEither :: (CVal a, CVal b) => CTy a -> CTy b -> CTy (Either a b)
  CVec    :: CVal a => CTy a -> CTy [a]

eraseTy :: CTy a -> ECTy
eraseTy CUnit         = ECUnit
eraseTy CInt          = ECInt
eraseTy CBool         = ECBool
eraseTy CFlt          = ECFlt
eraseTy CDbl          = ECDbl
eraseTy CCplx         = ECCplx
eraseTy CStr          = ECStr
eraseTy (CPair   l r) = ECPair (eraseTy l) (eraseTy r)
eraseTy (CEither l r) = ECEither (eraseTy l) (eraseTy r)
eraseTy (CVec    l  ) = ECVec (eraseTy l)

getTy :: forall a t. CVal a => t a -> ECTy
getTy _ = eraseTy (getCTy :: CTy a)

getCTyR :: CTy a -> TypeRep a
getCTyR (CPair _ _) = typeRep
getCTyR (CEither _ _) = typeRep
getCTyR (CVec _) = typeRep
getCTyR CUnit = typeRep
getCTyR CInt = typeRep
getCTyR CBool = typeRep
getCTyR CFlt = typeRep
getCTyR CDbl = typeRep
getCTyR CCplx = typeRep
getCTyR CStr = typeRep

eitherTy :: (CVal a, CVal b) => t a -> t b -> TypeRep (Either a b)
eitherTy _ _ = typeRep

pairTy :: (CVal a, CVal b) => t a -> t b -> TypeRep (a, b)
pairTy _ _ = typeRep

cTySpec :: forall a t st. CVal a => t a -> CGen st ([CDeclSpec], [CDerivedDeclr])
cTySpec _ = typeSpec $ eraseTy $ (getCTy :: CTy a)

type family IsFun f where
  IsFun (a -> b) = 'True
  IsFun a = 'False

class (Eq a, Ord a, Show a, Typeable a, IsFun a ~ 'False) => CVal a where
  getCTy  :: CTy a
  cVal    :: a -> CGen st CExpr

instance CVal () where
  getCTy = CUnit
  cVal () = cTySpec CUnit *> pure (cVar cUnit)

instance CVal Int where
  getCTy = CInt
  cVal i = pure $ cInt i

instance CVal Bool where
  getCTy = CBool
  cVal True = pure $ cInt 1
  cVal False = pure $ cInt 0

instance CVal Float where
  getCTy = CFlt
  cVal i = pure $ cFlt i

instance CVal Double where
  getCTy = CDbl
  cVal i = pure $ cDbl i

instance Ord (Complex Double) where
  compare i j =
    case compare (realPart i) (realPart j) of
      EQ -> compare (imagPart i) (imagPart j)
      o -> o

instance CVal (Complex Double) where
  getCTy = CCplx
  cVal i = pure $
    CBinary CAddOp (cDbl (realPart i))
    (CBinary CMulOp (cDbl (imagPart i)) (cVar $ internalIdent "I") undefNode)
    undefNode

instance (CVal a, CVal b) => CVal (a, b) where
  getCTy = CPair getCTy getCTy
  cVal (x, y) = do
    vx <- cVal x
    vy <- cVal y
    (t, q) <- cTySpec $ getCTy @(a,b)
    cVar <$> newVar t q (Just $ ini [mkInit vx, mkInit vy])
      where
        mkInit e = ([], CInitExpr e undefNode)

instance (CVal a, CVal b) => CVal (Either a b) where
  getCTy = CEither getCTy getCTy
  cVal (Left x) = do
    vx <- cVal x
    (t, q) <- cTySpec $ getCTy @(Either a b)
    cVar <$> newVar t q (Just $ ini [tagL, ([], ini [([mkInL], iniE vx)])])
      where
        tagL = ([], iniE $ cVar cTagl)
        mkInL = CMemberDesig inlFld undefNode
  cVal (Right x) = do
    vx <- cVal x
    (t, q) <- cTySpec $ getCTy @(Either a b)
    cVar <$> newVar t q (Just $ ini [tagR, ([], ini [([mkInR], iniE vx)])])
      where
        tagR = ([], iniE $ cVar cTagr)
        mkInR = CMemberDesig inrFld undefNode

ini :: CInitializerList NodeInfo -> CInitializer NodeInfo
ini = (`CInitList` undefNode)
iniE :: CExpression NodeInfo -> CInitializer NodeInfo
iniE = (`CInitExpr` undefNode)

instance (CVal a) => CVal [a] where
  getCTy = CVec getCTy
  cVal _xs = error "FIXME: unimplemented"

instance {-# OVERLAPPING #-} CVal String where
  getCTy = CStr
  cVal s = pure $ cStr s

(<::) :: forall a t st. CVal a => Ident -> t a -> CGen st [CBlockItem]
v <:: _ = do
  (t, q) <- cTySpec (getCTy :: CTy a)
  pure $ [CBlockDecl $ varDecl v t q Nothing]

cCase :: CExpr -> [CBlockItem] -> [CBlockItem] -> CStat
cCase e l r = CSwitch e cases undefNode
  where
    cases = cComp $ zipWith3 mkCase [True, False] [l, r] [cTagl, cTagr]
    mkCase bb ss tag = CBlockStmt $ CCase (cVar tag) (caseCode bb ss) undefNode
    caseCode b ss = cComp $ ss ++ mbreak
      where
        mbreak = if b then [CBlockStmt $ CBreak undefNode] else []

declVar :: forall a t st. CVal a => t a -> CGen st (CExpr, [CBlockItem])
declVar e
  | getTy e == ECUnit = pure (cVar cUnit, [])
  | otherwise = do
      v <- freshVar
      dv <- v <:: e
      pure (cVar v, dv)
