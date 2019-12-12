{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Language.SessionTypes.Global
  ( GT (..)
  , GTM
  , roles
  , unrollG
  , showGT
  , printGT
  , latexGT
  , message
  , send
  , recv
  , grec
  , gclose
  , mkRole
  , mkLabel
  ) where

import Control.Monad.State.Strict
import Data.Set ( Set )
import qualified Data.Set as Set
-- import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Text.Prettyprint.Doc ( Doc, Pretty, pretty )
import qualified Data.Text.Prettyprint.Doc as Pretty

import Language.SessionTypes.Common

class LaTeX t where
  latex :: t -> Doc ann

instance LaTeX String where
  latex i = pretty i

data GT t
  = Choice Role Role (GT t) (GT t)
  | ChoiceL Role Role (GT t)
  | ChoiceR Role Role (GT t)
  | ND (GT t) (GT t)
  | Comm Role Role t (GT t)
  | GSend Role Role t (GT t)
  | GRecv Role Role t (GT t)
  | GRec String Integer (GT t)
  | GVar String
  | GEnd
  deriving Show

roleList :: [String]
roleList = bl ++ [ b ++ "_" ++ show i | i <- [(1::Integer)..], b <- bl ]
  where
    bl = ["p", "q", "r", "s"]

instance LaTeX Role where
  latex (Rol r) = pretty "\\role{" <> pretty (roleList !! fromInteger r) <> pretty "}"

instance LaTeX Label where
  latex (Lbl l) = pretty "l_" <> pretty l

instance Show t => LaTeX (Alt (GT t)) where
  latex (Alt m)
    = Pretty.braces $
      Pretty.hsep $
      Pretty.punctuate (pretty ";") $
      map lalt $
      Map.toList m
    where
      lalt (l, g) = latex l <> pretty "." Pretty.<+> latex g

instance Show t => LaTeX (GT t) where
  latex (Choice src dest g1 g2)
    = Pretty.align
      $! Pretty.vsep
      $! [ Pretty.hsep [ latex src
                       , pretty "\\gMsg"
                       , latex dest
                       ]
         , b
         ]
    where
      b = Pretty.braces $
          Pretty.hsep $
          Pretty.punctuate (pretty ";") $
          map lalt $
          [(Lbl 0, g1), (Lbl 1, g2)]
        where
          lalt (l, g) = latex l <> pretty "." Pretty.<+> latex g
  latex (ChoiceL src dest g1)
    = Pretty.align
      $! Pretty.vsep
      $! [ Pretty.hsep [ latex src
                       , pretty "\\gMsg"
                       , latex dest
                       ]
         , b
         ]
    where
      b = Pretty.braces $
          Pretty.hsep $
          Pretty.punctuate (pretty ";") $
          map lalt $
          [(Lbl 0, g1)]
        where
          lalt (l, g) = latex l <> pretty "." Pretty.<+> latex g
  latex (ChoiceR src dest g1)
    = Pretty.align
      $! Pretty.vsep
      $! [ Pretty.hsep [ latex src
                       , pretty "\\gMsg"
                       , latex dest
                       ]
         , b
         ]
    where
      b = Pretty.braces $
          Pretty.hsep $
          Pretty.punctuate (pretty ";") $
          map lalt $
          [(Lbl 1, g1)]
        where
          lalt (l, g) = latex l <> pretty "." Pretty.<+> latex g
  latex (ND g1 g2)
    = Pretty.align
      $! b
    where
      b = Pretty.braces $
          Pretty.hsep $
          Pretty.punctuate (pretty ";") $
          map lalt $
          [(Lbl 0, g1), (Lbl 1, g2)]
        where
          lalt (l, g) = latex l <> pretty "." Pretty.<+> latex g
  latex (GRec v _ x) = Pretty.hsep [ pretty "\\gFix"
                                    , pretty v <> pretty "."
                                    , latex x
                                    ]
  latex (GVar v) = pretty v
  latex GEnd = pretty "\\gEnd"
  latex c = Pretty.align $!
                    Pretty.vsep $!
                    Pretty.punctuate (pretty ".") $!
                    go c
    where
      go (Comm f t ty b) = msg : go b
        where
          msg = Pretty.hsep
                [ latex f
                , pretty "\\gMsg"
                , latex t
                , pretty "\\gTy{"
                , pretty (show ty)
                , pretty "}"
                ]
      go (ChoiceL f t b) = msg : go b ++ [pretty "}"]

        where
          msg = Pretty.hsep
                [ latex f
                , pretty "\\gMsg"
                , latex t
                , pretty "\\gTy{"
                , pretty (Lbl 0)
                , pretty "."
                ]
      go (ChoiceR f t b) = msg : go b ++ [pretty "}"]

        where
          msg = Pretty.hsep
                [ latex f
                , pretty "\\gMsg"
                , latex t
                , pretty "\\gTy{"
                , pretty (Lbl 1)
                , pretty "."
                ]
      go (GSend f t ty b) = msg : go b
        where
          msg = Pretty.hsep
                [ latex f
                , pretty "\\gSend"
                , latex t
                , pretty "\\gTy{"
                , pretty (show ty)
                , pretty "}"
                ]
      go (GRecv f t ty b) = msg : go b
        where
          msg = Pretty.hsep
                [ latex t
                , pretty "\\gRecv"
                , latex f
                , pretty "\\gTy{"
                , pretty (show ty)
                , pretty "}"
                ]
      go g          = [latex g]

instance Show t => Pretty (GT t) where
  pretty (Choice src dest g1 g2) = Pretty.align
                                   $! Pretty.vsep
                                   $! [ Pretty.hsep [ pretty src
                                                    , pretty "->"
                                                 , pretty dest
                                                 ]
                                      , b
                                      ]
    where
      b = Pretty.braces $
          Pretty.vsep $
          Pretty.punctuate (pretty ";") $
          map lalt $
          [(Lbl 0, g1), (Lbl 1, g2)]
        where
          lalt (l, g) = pretty l <> pretty "." Pretty.<+> pretty g
  pretty (ND g1 g2)
    = Pretty.align
      $! b
    where
      b = Pretty.braces $
          Pretty.vsep $
          Pretty.punctuate (pretty ";") $
          map lalt $
          [(Lbl 0, g1), (Lbl 1, g2)]
        where
          lalt (l, g) = pretty l <> pretty "." Pretty.<+> pretty g
  pretty (GRec v k x) = Pretty.hsep [ pretty "rec[" <> pretty k <> pretty "]"
                                  , pretty v Pretty.<> pretty "."
                                  , pretty x
                                  ]
  pretty (GVar v) = pretty v
  pretty GEnd = pretty "end"
  pretty c = Pretty.align $!
                     Pretty.vsep $!
                     Pretty.punctuate (pretty ".") $!
                     go c
    where
      go (Comm f t ty b) = msg : go b
        where
          msg = Pretty.hsep
                [ pretty f
                , pretty "->"
                , pretty t
                , pretty ":"
                , Pretty.parens $ Pretty.hsep [ pretty (show ty)
                                              ]
                ]
      go (ChoiceL f t b) = msg : go b
        where
          msg = Pretty.hsep
                [ pretty f
                , pretty "->"
                , pretty t
                , pretty ":"
                , Pretty.parens $ Pretty.hsep [ pretty (Lbl 0)
                                              ]
                ]
      go (ChoiceR f t b) = msg : go b
        where
          msg = Pretty.hsep
                [ pretty f
                , pretty "->"
                , pretty t
                , pretty ":"
                , Pretty.parens $ Pretty.hsep [ pretty (Lbl 1)
                                              ]
                ]
      go (GSend f t ty b) = msg : go b
        where
          msg = Pretty.hsep
                [ pretty f
                , pretty "~>"
                , pretty t
                , pretty ":"
                , Pretty.parens $ pretty (show ty)
                ]
      go (GRecv f t ty b) = msg : go b
        where
          msg = Pretty.hsep
                [ pretty t
                , pretty "<~"
                , pretty f
                , pretty ": ("
                , pretty (show ty)
                , pretty ")"
                ]
      go g          = [pretty g]

showGT :: Show t => GT t -> String
showGT = show . pretty

printGT :: Show t => GT t -> IO ()
printGT = print . pretty

latexGT :: Show t => GT t -> IO ()
latexGT = print . latex

data GTSt ty = GTSt
  { nextRole :: Role
  , nextLabel :: Label
  , nextVar :: [RVar]
  , globalType :: GT ty -> GT ty
  }

type GTM ty a = State (GTSt ty) a

-- message r r' typ
message :: Role -> Role -> ty -> GTM ty ()
message r1 r2 ty = do
  s <- get
  put s { globalType = globalType s . Comm r1 r2 ty }

send :: Role -> Role -> ty -> GTM ty ()
send r1 r2 ty = do
  s <- get
  put s { globalType = globalType s . GSend r1 r2 ty }

recv :: Role -> Role -> ty -> GTM ty ()
recv r1 r2 ty = do
  s <- get
  put s { globalType = globalType s . GRecv r1 r2 ty }

newRVar :: GTM t String
newRVar = do
  s <- get
  case nextVar s of
    [] -> error "empty variable generator"
    RVar v : vs -> do
      put s { nextVar = vs }
      pure v

newtype RVar = RVar String

instance Var RVar where
  varGen = map RVar vg1 ++
           [ RVar $ v ++ show i | i <- [1::Integer ..], v <- vg1 ]
    where
      vg1 = map (:[]) ['X'..'Z']


grec :: Integer -> (GTM t () -> GTM t ()) -> GTM t ()
grec k f = do
  v <- newRVar
  modify $ \s -> s { globalType = globalType s . GRec v k }
  f $ mkVar v
  where
    mkVar :: String -> GTM t ()
    mkVar v = do
      s <- get
      put s { globalType = \_ -> globalType s (GVar v) }

class Var a where
  varGen :: [a]

instance Var Integer where
  varGen = [0..]

instance Var String where
  varGen = vg1 ++ [ v ++ show i | i <- [1::Integer ..], v <- vg1 ]
    where
      vg1 = map (:[]) ['a'..'z']

gclose :: GTM t () -> GT t
gclose g = globalType (execState g initSt) GEnd

initSt :: GTSt t
initSt = GTSt { nextRole = Rol 0
              , nextLabel = Lbl 0
              , nextVar = varGen
              , globalType = id
              }

mkRole :: GTM t Role
mkRole = do
  s <- get
  put s { nextRole = inc $! nextRole s }
  pure $! nextRole s
  where
    inc (Rol i) = Rol $! i+1

mkLabel :: GTM t Label
mkLabel = do
  s <- get
  put s { nextLabel = inc $! nextLabel s }
  pure $! nextLabel s
  where
    inc (Lbl i) = Lbl $! i+1

roles :: GT t -> Set Role
roles GEnd = Set.empty
roles GVar{} = Set.empty
roles (GRec _ _ g) = roles g
roles (Comm f t _ g) = Set.unions [ Set.fromList $ [f, t]
                                   , roles g
                                   ]
roles (GSend f t _ g) = Set.unions [ Set.fromList $ [f, t]
                                     , roles g
                                     ]
roles (GRecv f t _ g) = Set.unions [ Set.fromList $ [f, t]
                                   , roles g
                                   ]
roles (Choice r rs g1 g2 )
  = Set.unions [ Set.fromList $ [r, rs]
               , roles g1
               , roles g2
               ]
roles (ChoiceL r rs g1 )
  = Set.unions [ Set.fromList $ [r, rs]
               , roles g1
               ]
roles (ChoiceR r rs g1 )
  = Set.unions [ Set.fromList $ [r, rs]
               , roles g1
               ]
roles (ND g1 g2 )
  = Set.unions [ roles g1
               , roles g2
               ]

subst :: String -> GT t -> GT t -> GT t
subst v g (Choice r1 rs g1 g2) = Choice r1 rs (subst v g g1) (subst v g g2)
subst v g (ChoiceL r1 rs g1) = ChoiceL r1 rs (subst v g g1)
subst v g (ChoiceR r1 rs g1) = ChoiceR r1 rs (subst v g g1)
subst v g (ND g1 g2) = ND (subst v g g1) (subst v g g2)
subst v g (Comm f t ty k) = Comm f t ty $! subst v g k
subst v g (GSend f t ty k) = GSend f t ty $! subst v g k
subst v g (GRecv f t ty k) = GRecv f t ty $! subst v g k
subst v g gr@(GRec v' i g')
  | v == v' = gr
  | otherwise = GRec v' i $! subst v g g'
subst v g gv@(GVar v')
  | v == v' = g
  | otherwise = gv
subst _ _ GEnd = GEnd

unrollG :: Integer -> String -> GT t -> GT t
unrollG i v g
  | i <= 0 = GEnd
  | otherwise = subst v (unrollG (i-1) v g) g
