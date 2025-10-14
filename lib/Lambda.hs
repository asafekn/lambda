module Lambda where

import Prelude hiding (lex, exp)

data Exp
  = Var String
  | Fun String Exp
  | Apply Exp Exp
  deriving Show


eval :: Exp -> Exp
eval exp = case exp of
  Apply left right ->
    case left of
      Apply _ _ -> Apply (eval left) right
      Var _ -> Apply left (eval right)
      Fun var term -> eval (subst var right term)
  Fun _ _ -> exp
  Var _ -> exp


subst :: String -> Exp -> Exp -> Exp
subst var term exp =
  case exp of
    Var v
      | var == v -> term
      | otherwise -> exp
    Fun arg body
      | var == arg -> exp
      | otherwise -> Fun arg (subst var term body)
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
