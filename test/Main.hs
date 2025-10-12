module Main (main) where

import Test.Hspec (hspec, describe, shouldBe, it)
import Lambda
import Prelude hiding (lex, exp)

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
