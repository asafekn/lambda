module Lambda where

import Prelude hiding (lex, exp)
import Control.Applicative ((<|>))

data Exp
  = Var String
  | Lam String Exp
  | Apply Exp Exp
  deriving (Show, Eq)


eval :: Exp -> Exp
eval exp = case exp of
  Apply left right ->
    case left of
      Apply _ _ -> Apply (eval left) right
      Var _ -> Apply left (eval right)
      Lam var term -> eval (subst var right term)
  Lam _ _ -> exp
  Var _ -> exp


subst :: String -> Exp -> Exp -> Exp
subst var term exp =
  case exp of
    Var v
      | var == v -> term
      | otherwise -> exp
    Lam arg body
      | var == arg -> exp
      | otherwise -> Lam arg (subst var term body)
    Apply left right ->
      Apply (subst var term left) (subst var term right)


data Token
  = TokenParentesisOpen     -- "("
  | TokenParentesisClose    -- ")"
  | TokenLambda             -- "\"
  | TokenArrow              -- "->"
  | TokenIdentifier String
  deriving (Show, Eq)


lex :: String -> [Token]
lex str = reverse $ go [] str
  where
  go :: [Token] -> String -> [Token]
  go prev xs =
    case xs of
      [] -> prev
      ' ' : rest -> go prev rest
      '(' : rest -> go (TokenParentesisOpen : prev) rest
      ')' : rest -> go (TokenParentesisClose : prev) rest
      '\\' : rest -> go (TokenLambda : prev) rest
      '-' : '>' : rest -> go (TokenArrow : prev) rest
      c : rest ->
        if 'a' <= c && c <= 'z' then
          let (identifier, rest') = span identifierChar (c : rest)
          in go (TokenIdentifier identifier : prev) rest'
        else error "Invalid function"


identifierChar :: Char -> Bool
identifierChar c =
  if 'a' <= c && c <= 'z' then True
  else if 'A' <= c && c <= 'Z' then True
  else if '0' <= c && c <= '9' then True
  else if c == '_' then True
  else False


parse :: [Token] -> Exp
parse tokens =
  case parseExp tokens of
    Just (exp , []) -> exp
    Just (_ , tokens') -> error $ "tokens left " <> (show $ length tokens')
    Nothing -> error "no parse"


parseExp :: [Token] -> Maybe (Exp, [Token])
parseExp tokens =
  parseLam tokens <|> parseApp tokens


parseLam :: [Token] -> Maybe (Exp, [Token])
parseLam tokens =
  case tokens of
    TokenLambda : TokenIdentifier name : TokenArrow : rest -> do
      (exp, tokens') <- parseExp rest
      return ((Lam name exp), tokens')
    _ -> Nothing


parseApp :: [Token] -> Maybe (Exp, [Token])
parseApp tokens = do
  (first, rest) <- parseAtom tokens
  parseApps first rest
  where
    parseApps left tokens' =
      case parseAtom tokens' of
        Just (right, rest') -> parseApps (Apply left right) rest'
        Nothing -> Just (left, tokens')



parseAtom :: [Token] -> Maybe (Exp, [Token])
parseAtom tokens =
  case tokens of
    (TokenIdentifier name) : rest -> Just (Var name, rest)
    TokenParentesisOpen : rest -> do
      (exp, rest') <- parseExp rest
      case rest' of
        TokenParentesisClose : rest'' -> Just (exp, rest'')
        _ -> Nothing
    _ -> Nothing
