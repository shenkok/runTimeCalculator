e1 :: Maybe Int
e1 = Just 3
e2 :: Maybe Int
e2 = Just 4

data Expr = Val Int | Div Expr Expr 

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (div x y)

expresion :: Expr
expresion = Div (Val 3) (Val 4)


eval :: Expr -> Maybe Int
eval (Val v) = Just v
eval (Div e1 e2) = do
    x1 <- eval e1
    x2 <- eval e2
    return (x1 + x2)

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) <= 3 = Just (left + n, right)
    | otherwise                     = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs ((left + n) - right) <= 3 = Just (left, right + n)
    | otherwise                     = Nothing

routine ::  Maybe Pole
routine = do
    start  <- return (0, 0)
    firts  <- landLeft 2 start
    second <- landRight 1 firts
    return second   