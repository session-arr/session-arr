-- FIXME: Refactor out of this monad the channels and message passing
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.CGen
  ( CGen
  , CGSt
  , ECTy(..)
  , PID
  , initCGSt
  , freshVar
  , freshN
  , cVar
  , cInt
  , cFlt
  , cDbl
  , cStr
  , cUnit
  , cTagl
  , cTagr
  , cAddr
  , inlFld
  , inrFld
  , tagFld
  , valFld
  , fstFld
  , sndFld
  , elemsFld
  , sizeFld
  , declare
  , newFun
  , newVar
  , varDecl
  , getChan
  , csend
  , crecv
  , cExpr
  , cAssign
  , cCall
  , cComp
  , cMember
  , generateFile
  , isDeclared
  , addComment
  , typeSpec
  , newHeaderFun
  , warn
  , ustate
  , getUstate
  , module X
  ) where

import Control.Monad.RWS.Strict
import Control.Monad.Extra ( whenM )
import Data.Char
import Data.Map.Strict ( Map )
import Data.List ( intersperse )
import qualified Data.Map.Strict as Map
import Language.C.Data as X
import Language.C.Syntax as X
import Language.C.Pretty
import System.IO ( hPutStrLn, stderr )

import Text.PrettyPrint ( render )

data CGErr

instance Show CGErr where
  show _ = "error"

newtype CGLog = CGLog { unCGLog :: [String] }
instance Show CGLog where
  show = concat . intersperse "\n" . unCGLog

instance Semigroup CGLog where
  l1 <> l2 = CGLog $ unCGLog l1 ++ unCGLog l2

instance Monoid CGLog where
  mempty = CGLog []
  mappend = (<>)

type PID = Integer

data ECTy
  = ECUnit
  | ECInt
  | ECBool
  | ECFlt
  | ECDbl
  | ECCplx
  | ECStr
  | ECPair ECTy ECTy
  | ECEither ECTy ECTy
  | ECVec ECTy
  deriving (Eq, Ord, Show)


cTyName :: String -> ECTy -> String -> Ident
cTyName pref t suff = internalIdent $ pref ++ tyNm t ++ suff
  where
    tyNm :: ECTy -> String
    tyNm ECUnit = "unit"
    tyNm ECInt = "int"
    tyNm ECBool = "int"
    tyNm ECFlt = "float"
    tyNm ECDbl = "double"
    tyNm ECCplx = "cplx"
    tyNm ECStr = "string"
    tyNm (ECPair l r) = "pair_" ++ tyNm l ++ "_" ++ tyNm r
    tyNm (ECEither l r) = "either_" ++ tyNm l ++ "_" ++ tyNm r
    tyNm (ECVec a) = "vec_" ++ tyNm a


tUnit :: Ident
tUnit = internalIdent "unit"

cUnit :: Ident
cUnit = internalIdent "Unit"

tyUnit :: Ident
tyUnit = internalIdent "unit_t"

tyCplx :: Ident
tyCplx = internalIdent "cplx_t"

unitTySpec :: [CDeclSpec]
unitTySpec = [ CTypeSpec $
               CEnumType (CEnum (Just tUnit) (Just [(cUnit, Nothing)])
                          [] undefNode) undefNode
             ]

cplxTySpec :: [CDeclSpec]
cplxTySpec = [ CTypeSpec $ CDoubleType undefNode
             , CTypeSpec $ CComplexType undefNode
             ]

pairTySpec :: Ident
           -> ([CDeclSpec], [CDerivedDeclr])
           -> ([CDeclSpec], [CDerivedDeclr])
           -> [CDeclSpec]
pairTySpec nm (tl, ql) (tr, qr) =
  [ CTypeSpec $
    CSUType (CStruct CStructTag (Just nm) (Just [p1, p2]) [] undefNode) undefNode
  ]
  where
    p1 = fld fstFld tl ql
    p2 = fld sndFld tr qr

fld :: Ident -> [CDeclSpec] -> [CDerivedDeclr] -> CDecl
fld s t q = CDecl t [(fldD, Nothing, Nothing)] undefNode
  where
    fldD = Just $ CDeclr (Just s) q Nothing [] undefNode

tagFld :: Ident
tagFld = internalIdent "tag"

valFld :: Ident
valFld = internalIdent "val"

inlFld :: Ident
inlFld = internalIdent "inl"

inrFld :: Ident
inrFld = internalIdent "inr"

fstFld :: Ident
fstFld = internalIdent "fst"

sndFld :: Ident
sndFld = internalIdent "snd"

tyTag :: Ident
tyTag = internalIdent "tag_t"

tTag :: Ident
tTag = internalIdent "tag"

cTagl :: Ident
cTagl = internalIdent "Inl"

cTagr :: Ident
cTagr = internalIdent "Inr"

tagTySpec :: [CDeclSpec]
tagTySpec =
  [CTypeSpec $ CEnumType (CEnum (Just tTag) cs [] undefNode) undefNode]
  where
    cs = Just [(cTagl, Nothing),(cTagr, Nothing)]

eitherTySpec :: Ident
             -> ([CDeclSpec], [CDerivedDeclr])
             -> ([CDeclSpec], [CDerivedDeclr])
             -> [CDeclSpec]
eitherTySpec nm (tl, ql) (tr, qr) =
  [ CTypeSpec $
    CSUType (CStruct CStructTag (Just nm) (Just [p1, p2]) [] undefNode) undefNode
  ]
  where
    p1 = fld tagFld [CTypeSpec $ CTypeDef tyTag undefNode] []
    p2 = fld valFld [CTypeSpec $ cunion] []
    cunion = CSUType csu undefNode
    csu = CStruct CUnionTag Nothing (Just [inj1, inj2]) [] undefNode
    inj1 = fld inlFld tl ql
    inj2 = fld inrFld tr qr

vecTySpec :: Ident
          -> ([CDeclSpec], [CDerivedDeclr])
          -> [CDeclSpec]
vecTySpec nm (tl, ql) =
  [ CTypeSpec $
    CSUType (CStruct CStructTag (Just nm) (Just [p1, p2]) [] undefNode) undefNode
  ]
  where
    p1 = fld elemsFld tl ql
    p2 = fld sizeFld [ CTypeSpec $ CTypeDef (internalIdent "size_t") undefNode
                     ] []

elemsFld :: Ident
elemsFld = internalIdent "elems"

sizeFld :: Ident
sizeFld = internalIdent "size"

typeSpec :: ECTy -> CGen st ([CDeclSpec], [CDerivedDeclr])
typeSpec ECUnit = (,) <$> declare tyUnit unitTySpec <*> pure []
typeSpec ECInt = pure $ ([CTypeSpec $ CIntType undefNode], [])
typeSpec ECBool = pure $ ([CTypeSpec $ CIntType undefNode], [])
typeSpec ECFlt = pure $ ([CTypeSpec $ CFloatType undefNode], [])
typeSpec ECDbl = pure $ ([CTypeSpec $ CDoubleType undefNode], [])
typeSpec ECCplx = (,[]) <$> declare tyCplx cplxTySpec
typeSpec ECStr = pure $ ([CTypeSpec $ CCharType undefNode], [CPtrDeclr [] undefNode])
typeSpec p@(ECPair l r) = join $ doPair <$> typeSpec l <*> typeSpec r
  where
    doPair cl cr = (,) <$> declare nmt (pairTySpec nm cl cr) <*> pure []
    nmt = cTyName "" p "_t"
    nm = cTyName "" p ""
typeSpec (ECEither ECUnit ECUnit) = (,[]) <$> declare tyTag tagTySpec
typeSpec p@(ECEither l r) = do
  void $ declare tyTag tagTySpec
  join $ doEither <$> typeSpec l <*> typeSpec r
  where
    doEither cl cr
      = (,[]) <$> declare nmt (eitherTySpec nm cl cr)
    nmt = cTyName "" p "_t"
    nm = cTyName "" p ""
typeSpec v@(ECVec a) =
  (,[]) <$> (declare nmt =<< vecTySpec nm . addPtr <$> typeSpec a)
  where
    addPtr (t, d) = (t, CPtrDeclr [] undefNode : d)
    nmt = cTyName "" v "_t"
    nm = cTyName "" v ""

data Chan = Chan { chname :: Ident
                 , chsend :: Ident
                 , chrecv :: Ident
                 }

data CGSt st =
  CGSt { varGen :: ![String] -- ^ Free variable generator

       , decls :: !(Map Ident CExtDecl) -- ^ Mapping from identifier to declaration
       , comm :: !(Map Ident String) -- ^ Comments to be added to final file
       , declOrder :: ![Ident]
       , channel :: !(Map (PID, PID, ECTy) Chan)

       , headerDecls :: !(Map Ident CExtDecl)
       , hdeclOrder :: !([Ident])
       , externalSt :: !st
       }

lookComm :: Ident -> CGSt st -> String
lookComm i st = maybe "" id $ Map.lookup i $ comm st

isDeclared :: Ident -> CGen st Bool
isDeclared i = do
  b1 <- Map.member i <$> gets decls
  b2 <- Map.member i <$> gets headerDecls
  pure $ b1 || b2

newtype CGen st a = CGen { unCGen :: RWS () CGLog (CGSt st) a }
  deriving (Functor, Applicative, Monad)

warn :: String -> CGen st ()
warn w = tell $ CGLog [w]

generateFile :: st -> FilePath -> CGen st () -> IO ()
generateFile ist fp m =
  case runRWS (unCGen m) () (initCGSt ist) of
    -- (Left err, _, clog) -> do
    --   hPutStrLn stderr $ show clog
    --   putStrLn $ "\n"
    --   hPutStrLn stderr $ show err
    (_, st, clog) -> do
      hPutStrLn stderr $ show clog
      writeFile (fp ++ ".c") $ serialise st ++ "\n"
      writeFile (fp ++ ".h") $ serialiseH st ++ "\n"
  where
    defined = "__" ++ map toUpper fp ++ "__"
    serialiseH st =
      "#ifndef " ++ defined ++ "\n" ++
      "#define " ++ defined ++ "\n\n" ++
      "#include<stdio.h>\n#include<stdlib.h>\n#include<pthread.h>\n#include<complex.h>\n" ++
      concat (intersperse "\n\n" $ map lookHdef $ reverse $ hdeclOrder st) ++
      "\n\n#endif\n"
      where
        lookHdef d = render (pretty $ headerDecls st Map.! d)
    serialise st
      = "#include \"" ++ fp ++ ".h\"\n\n" ++
        concat (intersperse "\n\n" $ map lookDef $ reverse $ declOrder st)
      where
        lookDef d = printComm (lookComm d st)  ++
          render (pretty $ decls st Map.! d)
        printComm "" = ""
        printComm cs = "/*\n * " ++ cs ++ "\n */\n"

deriving instance MonadReader () (CGen st)
deriving instance MonadWriter CGLog (CGen st)
deriving instance MonadState (CGSt st) (CGen st)

addComment :: Ident -> String -> CGen st ()
addComment i s = modify $ \st -> st { comm = Map.insert i s $ comm st }

initCGSt :: st -> CGSt st
initCGSt ist = CGSt { varGen = vgen
                    , decls = Map.empty
                    , comm = Map.empty
                    , declOrder = []
                    , channel = Map.empty
                    , headerDecls = Map.empty
                    , hdeclOrder = []
                    , externalSt = ist
                    }
  where
    vgen = map ("v_"++) gen
    gen = [[c] | c <- ['a'..'z']]
          ++ [ i : c | c <- gen, i <- ['a' .. 'z'] ]

newHeaderFun :: Ident
             -> ([CDeclSpec], [CDerivedDeclr])
             -> [([CDeclSpec], [CDerivedDeclr])]
             -> CGen st ()
newHeaderFun fn (dty, dq) args = modify $ \s ->
  s { headerDecls = Map.insert fn decl $ headerDecls s
    , hdeclOrder = fn : hdeclOrder s}
  where
    decl = CDeclExt $ CDecl dty [(Just fdeclr, Nothing, Nothing)] undefNode
    fdeclr = CDeclr (Just fn) (fndeclr : dq) Nothing [] undefNode
    fndeclr = CFunDeclr (Right (fnargs, False)) [] undefNode
    fnargs = map mkFnArg args
    mkFnArg (at, aq) = CDecl at [(Just $ mkArgD aq, Nothing, Nothing)] undefNode
    mkArgD aq = CDeclr Nothing aq Nothing [] undefNode


freshVar :: CGen st Ident
freshVar = gets varGen >>= \(h:t) -> do
  ds <- gets decls
  let nh = internalIdent h
  if nh `Map.member` ds
    then modify (\s -> s { varGen = t }) *> freshVar
    else pure (internalIdent h) <* modify (\s -> s { varGen = t} )

freshN :: String -> CGen st Ident
freshN f = go (Nothing :: Maybe Int) <$> gets decls
  where
    fullName Nothing  = f
    fullName (Just i) = f ++ "_" ++ show i
    go m ds =
      let nh = internalIdent $ fullName m in
      if nh `Map.member` ds
        then go (maybe (Just 0) (\i -> Just (i+1)) m) ds
        else nh

ustate :: (st -> st) -> CGen st ()
ustate f = modify $ \s -> s { externalSt = f $ externalSt s }

getUstate :: (st -> a) -> CGen st a
getUstate f = f <$> gets externalSt

cVar :: Ident -> CExpr
cVar s = CVar s undefNode

cInt :: Int -> CExpr
cInt i = CConst $ CIntConst (cInteger $ fromIntegral i) undefNode

cFlt :: Float -> CExpr
cFlt i = CConst $ CFloatConst (cFloat i) undefNode

cDbl :: Double -> CExpr
cDbl i = CConst $ CFloatConst (cFloat $ realToFrac i) undefNode

cStr :: String -> CExpr
cStr s = CConst $ CStrConst (cString s) undefNode


declare :: Ident -> [CDeclSpec] -> CGen st [CDeclSpec]
declare nm cd = do
  s <- get
  if Map.member nm $ headerDecls s
    then pure $ [CTypeSpec $ CTypeDef nm undefNode]
    else do put s { headerDecls = Map.insert nm (CDeclExt dd) $ headerDecls s
                  , hdeclOrder = nm : hdeclOrder s
                  }
            pure $ [CTypeSpec $ CTypeDef nm undefNode]
  where
    dd = CDecl (CStorageSpec (CTypedef undefNode) : cd)
         [(Just (CDeclr (Just nm) [] Nothing [] undefNode), Nothing, Nothing)]
         undefNode

newFun :: (Ident, ([CDeclSpec], [CDerivedDeclr]))
       -> [(Ident, ([CDeclSpec], [CDerivedDeclr]))]
       -> [CBlockItem]
       -> CGen st ()
newFun (f, (rty, fq)) xs body =
  modify $ \s -> s { decls = Map.insert f (CFDefExt fdef) $ decls s
                   , declOrder = f : declOrder s
                   }
  where
    fdef = CFunDef rty fdeclr [] cbody undefNode
    fdeclr = CDeclr (Just f) (fundeclr : fq) Nothing [] undefNode
    cbody = CCompound [] body undefNode
    fundeclr = CFunDeclr (Right (map arg xs, False)) [] undefNode
    arg (a, (aty, aq)) =
      CDecl aty [(Just adeclr, Nothing, Nothing)] undefNode
      where
        adeclr = CDeclr (Just a) aq Nothing [] undefNode

declVar :: Ident -> [CDeclSpec] -> [CDerivedDeclr] -> Maybe CInit -> CGen st ()
declVar v t q i =
  modify $ \s ->
    s { decls = Map.insert v (CDeclExt $ varDecl v t q i) $ decls s
      , declOrder = v : declOrder s
      }

newVar :: [CDeclSpec] -> [CDerivedDeclr] -> Maybe CInit -> CGen st Ident
newVar t q i = do
  v <- freshVar
  modify $ \s ->
    s { decls = Map.insert v (CDeclExt $ varDecl v t q i) $ decls s
      , declOrder = v : declOrder s
      }
  pure v

varDecl :: Ident -> [CDeclSpec] -> [CDerivedDeclr] -> Maybe CInit -> CDecl
varDecl v t q i = CDecl t [(Just dc, i, Nothing)] undefNode
  where
    dc = CDeclr (Just v) q Nothing [] undefNode

{-
typedef struct qsname
{ unsigned int q_size;
  unsigned int q_head;
  unsigned int q_tail;
pthread_mu
  cty q_mem[NUM_ELEMS];
}

-}

chanTySpec :: Ident
           -> ([CDeclSpec], [CDerivedDeclr])
           -> [CDeclSpec]
chanTySpec nm (tl, ql) =
  [ CTypeSpec $
    CSUType (CStruct CStructTag (Just nm) (Just qFields) [] undefNode) undefNode
  ]
  where
    qFields =
      [ fld qsizeFld [ CTypeQual $ CVolatQual undefNode
                     , CTypeSpec $ CUnsigType undefNode
                     , CTypeSpec $ CIntType undefNode] []
      , fld qheadFld [ CTypeSpec $ CIntType undefNode] []
      , fld qtailFld [ CTypeSpec $ CIntType undefNode] []
      , fld qmutexFld [ CTypeSpec $
                        CTypeDef (internalIdent "pthread_mutex_t")
                        undefNode
                      ] []
      , fld qfullFld [ CTypeSpec $
                       CTypeDef (internalIdent "pthread_cond_t")
                       undefNode
                     ] []
      , fld qemptyFld [ CTypeSpec $
                       CTypeDef (internalIdent "pthread_cond_t")
                       undefNode
                     ] []
      , fld qmemFld tl (CArrDeclr [] (CArrSize False qSize) undefNode : ql)
      ]



qsizeFld :: Ident
qsizeFld = internalIdent "q_size"

qheadFld :: Ident
qheadFld = internalIdent "q_head"

qtailFld :: Ident
qtailFld = internalIdent "q_tail"

qmutexFld :: Ident
qmutexFld = internalIdent "q_mutex"

qfullFld :: Ident
qfullFld = internalIdent "q_full"

qemptyFld :: Ident
qemptyFld = internalIdent "q_empty"

qmemFld :: Ident
qmemFld = internalIdent "q_mem"

-- qfullFld :: Ident
-- qfullFld = internalIdent "q_full"

-- TODO: optimize channel type
getChan :: PID -> PID -> ECTy -> CGen st Chan
getChan from to ty = do
  s <- get
  case Map.lookup (from, to, ty) (channel s) of
    Just i -> pure i
    Nothing -> do
      cty <- typeSpec ty
      ctys <- declare qtname (chanTySpec qsname cty)

      let sz = show $ Map.size (channel s)
          chn = internalIdent $ "ch" ++ sz
          sendc = internalIdent $ identToString qsname ++ "_put"
          recvc = internalIdent $ identToString qsname ++ "_get"
          ch = Chan { chname = chn, chsend = sendc, chrecv = recvc }

      declVar chn ctys [] (Just initChan)

      whenM (not <$> isDeclared sendc) $ do
        v <- freshVar
        vch <- freshN "ch"
        let sB = mkSendc (cVar vch) v
        newFun (sendc, ([CTypeSpec $ CVoidType undefNode], []))
          [ (vch, (ctys, [CPtrDeclr [] undefNode]))
          , (v, cty)
          ] sB
        newHeaderFun sendc ([CTypeSpec $ CVoidType undefNode], [])
          [ (ctys, [CPtrDeclr [] undefNode]) , cty]

      whenM (not <$> isDeclared recvc) $ do
        vr <- freshVar
        vch <- freshN "ch"
        newFun (recvc, cty) [(vch, (ctys, [CPtrDeclr [] undefNode]))] $ mkRecvc (cVar vch) vr cty
        newHeaderFun recvc cty [(ctys, [CPtrDeclr [] undefNode])]

      modify $ \st -> st { channel = Map.insert (from, to, ty) ch $ channel s }

      pure ch
  where
    qsname = cTyName "q_" ty ""
    qtname = cTyName "q_" ty "_t"

mkSendc :: CExpr -> Ident -> [CBlockItem]
mkSendc ch v =
  [ CBlockStmt $ cExpr $ CCall pthreadMutexLock [cAddr mutex] undefNode
  , CBlockStmt $
    CWhile (intConst 1) (CCompound [] body undefNode) False undefNode
  ]
  where
    body = map CBlockStmt
      [ CWhile isFull wait False undefNode
      , CIf notFull (CCompound [] addToQ undefNode) Nothing undefNode
      ]
    addToQ = map CBlockStmt
      [ cExpr $ CAssign CAssignOp (cIdx qmem qhd) (cVar v) undefNode
      , cExpr $ CAssign CAssignOp qhd incIdx undefNode
      , cExpr $ CUnary CPostIncOp qsz undefNode
      , cExpr $ CCall pthreadMutexSignal [cAddr qempty] undefNode
      , cExpr $ CCall pthreadMutexUnlock [cAddr mutex] undefNode
      , CReturn Nothing undefNode
      ]
    wait = CCompound []
      [ CBlockStmt $ cExpr $ CCall pthreadMutexWait [cAddr qfull, cAddr mutex]
        undefNode
      ] undefNode
    incIdx = (qhd `cPlus` intConst 1) `cMod` qSize
    qhd = cMemberAddr ch qheadFld
    qsz = cMemberAddr ch qsizeFld
    qmem = cMemberAddr ch qmemFld
    isFull = cMemberAddr ch qsizeFld `cGeq` qSize
    notFull = cMemberAddr ch qsizeFld `cLt` qSize
    mutex = cMemberAddr ch qmutexFld
    qfull = cMemberAddr ch qfullFld
    qempty = cMemberAddr ch qemptyFld

cIdx :: CExpr -> CExpr -> CExpr
cIdx e1 e2 = CIndex e1 e2 undefNode

pthreadMutexLock :: CExpr
pthreadMutexLock = cVar $ internalIdent "pthread_mutex_lock"

pthreadMutexUnlock :: CExpr
pthreadMutexUnlock = cVar $ internalIdent "pthread_mutex_unlock"

pthreadMutexWait :: CExpr
pthreadMutexWait = cVar $ internalIdent "pthread_cond_wait"

pthreadMutexSignal :: CExpr
pthreadMutexSignal = cVar $ internalIdent "pthread_cond_signal"

--emptyStat :: CStat
--emptyStat = CCompound [] [] undefNode

cGeq :: CExpr -> CExpr -> CExpr
cGeq e1 e2 = CBinary CGeqOp e1 e2 undefNode

cLeq :: CExpr -> CExpr -> CExpr
cLeq e1 e2 = CBinary CLeqOp e1 e2 undefNode

cGt :: CExpr -> CExpr -> CExpr
cGt e1 e2 = CBinary CGrOp e1 e2 undefNode

cPlus :: CExpr -> CExpr -> CExpr
cPlus e1 e2 = CBinary CAddOp e1 e2 undefNode

cMod :: CExpr -> CExpr -> CExpr
cMod e1 e2 = CBinary CRmdOp e1 e2 undefNode

cLt :: CExpr -> CExpr -> CExpr
cLt e1 e2 = CBinary CLeOp e1 e2 undefNode

-- void send_ch0(tag_t v_c1)
-- {
--   while (1) {
--     while (ch0.q_size >= 10);
--     pthread_mutex_lock(&ch0.q_mutex);
--     if (ch0.q_size < 10) {
--       ch0.q_mem[ch0.q_head] = v_c1;
--       ch0.q_head = (ch0.q_head + 1) % 10;
--       ch0.q_size++
--       pthread_mutex_unlock(&ch0.q_mutex);
--       return;
--     }
--     pthread_mutex_unlock(&ch0.q_mutex);
--   }
-- }


-- tag_t recv_ch0()
-- {
--   while (1) {
--     while (ch0.q_size <= 0);
--     pthread_mutex_lock(&ch0.q_mutex);
--     if (ch0.q_size > 0) {
--       tag_t res = ch0.q_mem[ch0.q_tail];
--       ch0.q_tail = (ch0.q_tail + 1) % 10;
--       ch0.q_size--;
--       pthread_mutex_unlock(&ch0.q_mutex);
--       return res;
--     }
--     pthread_mutex_unlock(&ch0.q_mutex);
--   }
-- }
--

mkRecvc :: CExpr -> Ident -> ([CDeclSpec], [CDerivedDeclr]) -> [CBlockItem]
mkRecvc ch v (tyd, tyq) =
  [ CBlockDecl $ CDecl tyd [(Just vdecl, Nothing, Nothing)] undefNode
  , CBlockStmt $ cExpr $ CCall pthreadMutexLock [cAddr mutex] undefNode
  , CBlockStmt $
    CWhile (intConst 1) (CCompound [] body undefNode) False undefNode
  ]
  where
    vdecl = CDeclr (Just v) tyq Nothing [] undefNode
    body = map CBlockStmt
      [ CWhile isEmpty wait False undefNode
      , CIf notEmpty (CCompound [] getFromQ undefNode) Nothing undefNode
      ]
    getFromQ = map CBlockStmt
      [ cExpr $ CAssign CAssignOp (cVar v) (cIdx qmem qtl) undefNode
      , cExpr $ CAssign CAssignOp qtl incIdx undefNode
      , cExpr $ CUnary CPostDecOp qsz undefNode
      , cExpr $ CCall pthreadMutexSignal [cAddr qfull] undefNode
      , cExpr $ CCall pthreadMutexUnlock [cAddr mutex] undefNode
      , CReturn (Just $ cVar v) undefNode
      ]
    wait = CCompound []
      [ CBlockStmt $ cExpr $ CCall pthreadMutexWait [cAddr qempty, cAddr mutex]
        undefNode
      ] undefNode
    incIdx = (qtl `cPlus` intConst 1) `cMod` qSize
    qtl = cMemberAddr ch qtailFld
    qsz = cMemberAddr ch qsizeFld
    qmem = cMemberAddr ch qmemFld
    isEmpty = cMemberAddr ch qsizeFld `cLeq` intConst 0
    notEmpty = cMemberAddr ch qsizeFld `cGt` intConst 0
    mutex = cMemberAddr ch qmutexFld
    qfull = cMemberAddr ch qfullFld
    qempty = cMemberAddr ch qemptyFld

intConst :: Integer -> CExpr
intConst i = CConst $ CIntConst (cInteger i) undefNode

qSize :: CExpr
qSize = CConst $ CIntConst (cInteger 1) undefNode

initChan :: CInit
initChan = CInitList
  [ ([],CInitExpr (CConst $ CIntConst (cInteger 0) undefNode) undefNode)
  , ([],CInitExpr (CConst $ CIntConst (cInteger 0) undefNode) undefNode)
  , ([],CInitExpr (CConst $ CIntConst (cInteger 0) undefNode) undefNode)
  -- , ([],CInitExpr (cVar $ internalIdent "PTHREAD_MUTEX_INITIALIZER") undefNode)
  , ([],CInitList [] undefNode)
  ] undefNode

csend :: PID -> PID -> ECTy -> CExpr -> CGen st [CBlockItem]
csend from to ty v = do
  c <- getChan from to ty
  pure $ [CBlockStmt $ cExpr $
    CCall (cVar $ chsend c) [cAddr $ cVar $ chname c, v] undefNode]

cAddr :: CExpr -> CExpr
cAddr e = CUnary CAdrOp e undefNode

crecv :: PID -> PID -> ECTy -> CExpr -> CGen st [CBlockItem]
crecv from to ty v = do
  c <- getChan to from ty
  pure $ [ CBlockStmt $ cExpr $
           cAssign v $ CCall (cVar $ chrecv c) [cAddr $ cVar $ chname c] undefNode ]

cExpr :: CExpr -> CStat
cExpr e = CExpr (Just e) undefNode

cAssign :: CExpr -> CExpr -> CExpr
cAssign vv e = CAssign CAssignOp vv e undefNode

cCall :: Ident -> [CExpr] -> CExpr
cCall fn e = CCall (cVar fn) e undefNode

cMember :: CExpr -> Ident -> CExpr
cMember e i = CMember e i False undefNode

cMemberAddr :: CExpr -> Ident -> CExpr
cMemberAddr e i = CMember e i True undefNode

cComp :: [CBlockItem] -> CStat
cComp e = CCompound [] e undefNode
