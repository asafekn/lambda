module Main where

import Lambda
import Prelude hiding (lex)
import System.IO
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if args == [] then 
    runRepl 
  else 
    runFile (head args)

runFile :: FilePath -> IO ()
runFile f = do
  content <- readFile f
  putStrLn (show $ eval (parse (lex content)))

runRepl :: IO ()
runRepl = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Lambda repl"
  putStr "lambda> "
  interact (concat . fmap repl . lines)
  where
    repl str = (show $ eval (parse (lex str))) <> "\nlambda> "