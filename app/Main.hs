module Main where

import Lambda
import Prelude hiding (lex)
import System.IO
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    file : _ -> runFile file

runFile :: FilePath -> IO ()
runFile f = do
  content <- readFile f
  putStrLn (show $ evalProgram (parse (lex content)))

runRepl :: IO ()
runRepl = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Lambda repl"
  putStr "lambda> "
  interact (concat . fmap repl . lines)
  where
    repl str = (show $ evalProgram (parse (lex str))) <> "\nlambda> "
