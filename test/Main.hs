module Main (main) where

import Test.Hspec (hspec, describe, shouldBe, it, shouldThrow, errorCall)
import Lambda
import Prelude hiding (lex, exp)
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "lexing" $ do
    it "function" $
      lex "(\\ x -> x)" `shouldBe`
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

