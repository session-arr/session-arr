module Main where

import Language.SPar.Compiler
import System.Environment
import System.Exit

import System.Console.GetOpt

data Flag
  = Infer String
  deriving Show

options :: [OptDescr Flag]
options =
 [ Option ['i']     ["infer"]  (ReqArg Infer "FUNCTION")  "infer FUNCTION"
 ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: <command> [OPTION...] files..."

main :: IO ()
main = getArgs >>= compilerOpts >>= \(args, fps) ->
  case fps of
    [] -> die "Error: no input file"
    (fp:_) -> case args of
                (Infer f : _) -> mpst fp [f]
                _ -> compile fp []
