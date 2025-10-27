{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Test.Hspec (hspec, describe, shouldBe, it, shouldThrow, errorCall)
import Lambda
import Prelude hiding (lex, exp)
import Control.Exception (evaluate)
import Data.String.Interpolate (i)

main :: IO ()
main = hspec $ do
  describe "lexing" $ do
    it "function" $
      lex [i|(\\ x -> x)|] `shouldBe`
        [TokenParentesisOpen,
        TokenLambda,
        TokenIdentifier "x",
        TokenArrow,
        TokenIdentifier "x",
        TokenParentesisClose]

    it "function with identifier diferent name" $
      lex "(\\ identifier_One -> identifier_One)" `shouldBe`
        [TokenParentesisOpen,
        TokenLambda,
        TokenIdentifier "identifier_One",
        TokenArrow,
        TokenIdentifier "identifier_One",
        TokenParentesisClose]

    it "variable can't start with upper case letter" $ do
      (evaluate $ lex "(\\ I -> I)") `shouldThrow` errorCall "Invalid function"

  describe "parsing" $ do
    it "parse variable" $
      parse (lex "x") `shouldBe`
        Var "x"

    it "parse lambda" $
      parse (lex "\\x -> x") `shouldBe`
        Lam "x" (Var "x")

    it "parse lambda with different var" $
      parse (lex "\\x -> y") `shouldBe`
        Lam "x" (Var "y")

    it "parse application of two variables" $
      parse (lex "x y") `shouldBe`
        Apply (Var "x") (Var "y")

    it "parse application of function with argument" $
      parse (lex "(\\x -> x) y") `shouldBe`
        Apply (Lam "x" (Var "x")) (Var "y")

    it "parse tree of application" $
      parse (lex "x y z") `shouldBe`
        Apply (Apply (Var "x") (Var "y")) (Var "z")

    it "parse lambda with parentheses application" $
      parse (lex "(\\x -> (x y))") `shouldBe`
        Lam "x" (Apply (Var "x") (Var "y"))

  describe "evaluate" $ do
    it "simple function application" $
      eval (parse (lex "(\\ x -> x) y")) `shouldBe`
        Var "y"
