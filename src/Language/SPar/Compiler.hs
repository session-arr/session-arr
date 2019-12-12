{-# LANGUAGE TupleSections #-}
module Language.SPar.Compiler
  ( compile
  , mpst
  ) where

import Data.List ( intersperse, foldl' )
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe
import System.FilePath
-- import System.Exit

compile :: FilePath -> [String] -> IO ()
compile fp _args = do
  r <- runInterpreter loadPar
  case r of
    Left err ->
      case err of
        UnknownError s -> putStrLn $ "Unknown error: " ++ s
        WontCompile es -> putStrLn $ "Compilation error: " ++
                          concat (intersperse "\n" $ map errMsg es)
        NotAllowed s -> putStrLn $ "Not allowed: " ++ s
        GhcException s -> putStrLn $ "Exception: " ++ s

    Right () -> pure ()
  where
    loadPar :: InterpreterT IO ()
    loadPar = do
      unsafeSetGhcOption "-fconstraint-solver-iterations=0"
      loadModules [fp]
      setTopLevelModules [takeBaseName fp]

      setImports [ "System.Environment"
                 , "Language.SPar.Skel"
                 , "Control.Monad.CGen"
                 , "Control.Monad"
                 , "Prelude"
                 ]
      fns <- getModuleExports $ takeBaseName fp
      ftys <- (map fst . filter snd) <$>
        mapM (\e ->
                case e of
                  Fun nm -> (nm,) <$> typeChecks (compileCommand nm)
                  _ -> pure ("", False)
             ) fns

      liftIO $ putStrLn $ "Found functions: "
      liftIO $ mapM_ putStrLn ftys
      compileAll $ ftys

    compileAll :: [String] -> InterpreterT IO ()
    compileAll fns = runStmt command
      where
        command = "withProgName " ++ show (takeBaseName fp)
          ++ " (generateFile emptyASt \"" ++ (takeBaseName fp ++ "\" $ ")
          ++ foldl' (\a b -> a ++ " >> " ++ compileCommand b) "pure ()" fns
          ++ ")"

mpst :: FilePath -> [String] -> IO ()
mpst fp args = do
  r <- runInterpreter loadPar
  case r of
    Left err ->
      case err of
        UnknownError s -> putStrLn $ "Unknown error: " ++ s
        WontCompile es -> putStrLn $ "Compilation error: " ++
                          concat (intersperse "\n" $ map errMsg es)
        NotAllowed s -> putStrLn $ "Not allowed: " ++ s
        GhcException s -> putStrLn $ "Exception: " ++ s

    Right () -> pure ()
  where
    loadPar :: InterpreterT IO ()
    loadPar = do
      unsafeSetGhcOption "-fconstraint-solver-iterations=0"
      loadModules [fp]
      setTopLevelModules [takeBaseName fp]

      setImports [ "System.Environment"
                 , "Language.SPar.Skel"
                 , "Control.Monad.CGen"
                 , "Control.Monad"
                 , "Prelude"
                 ]
      fns <- getModuleExports $ takeBaseName fp
      ftys <- (map fst . filter snd) <$>
        mapM (\e ->
                case e of
                  Fun nm -> if nm `elem` args
                            then (nm,) <$> typeChecks (inferCmd nm)
                            else pure ("", False)
                  _ -> pure ("", False)
             ) fns

      gtAll $ ftys

    gtAll :: [String] -> InterpreterT IO ()
    gtAll fns = mapM_ (\fn -> runStmt (command fn)) fns
      where
        command fn = "generateGtFile" ++ show (takeBaseName fp ++ "_" ++ fn)
          ++ " [" ++ inferCmd fn ++ "]"
    inferCmd nm = "(" ++ show nm ++ ", inferGt  mempty " ++ nm ++ ")"


compileCommand :: [Char] -> [Char]
compileCommand nm = "compileAsLib "++ show nm ++ " mempty " ++ nm
