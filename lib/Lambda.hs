module Lambda where


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
