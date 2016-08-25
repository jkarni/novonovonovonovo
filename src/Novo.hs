module Novo where


data Expr
  = Soma Expr Expr
  | Mult Expr Expr
  | Menos Expr Expr
  | Lit Int
  deriving (Show, Read, Eq)


notEqual :: forall a. (Eq a) => a -> a -> Bool
notEqual a b = not (a == b)

class Gen a where
  gen :: a

instance Gen Int where
  gen = 42

instance Gen String where
  gen = "Hello"

prop :: Gen a => (a -> Bool) -> Bool
prop fn = fn gen

eval :: Expr -> Expr
eval (Lit a) = Lit a
eval (Soma a b) = case (eval a, eval b) of
  (Lit a', Lit b') -> Lit (a' + b')
  _ -> error "Cannot happen"
eval (Mult a b) = case (eval a, eval b) of
  (Lit a', Lit b') -> Lit (a' * b')
  _ -> error "Cannot happen"
eval (Menos a b) = case (eval a, eval b) of
  (Lit a', Lit b') -> Lit (a' - b')
  _ -> error "Cannot happen"
