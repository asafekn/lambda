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

    it "function with identifier diferent names" $
      lex "(\\ identifier_One -> identifier_One)" `shouldBe`
        [TokenParentesisOpen,
        TokenLambda,
        TokenIdentifier "identifier_One",
        TokenArrow,
        TokenIdentifier "identifier_One",
        TokenParentesisClose]

    it "lex shuld not work, because of the first Caps char lol" $
      lex "(\\ Identifier_Two -> Identifier_Two)" `shouldBe` Error "invalid function"
