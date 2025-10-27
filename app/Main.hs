module Main where

import Lambda
import Prelude hiding (lex)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Lambda repl"
  putStr "lambda> "
  interact (concat . fmap repl . lines)
  where
  repl str = (show $ eval (parse (lex str))) <> "\nlambda> "
