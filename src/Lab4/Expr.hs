module Expr
  ( -- data definitions
    Expr (..),
    -- constructors
    x,
    num,
    add,
    mul,
    Expr.sin,
    Expr.cos,
    -- examples
    ex1,
    ex2,
    ex3,
    ex4,
    -- utility functions
    size,
    showExpr,
    eval,
    readExpr,
    prop_ShowReadExpr,
    arbExpr,
    simplify,
    differentiate,
  )
where

import Test.QuickCheck

-- | data definitions ---------------------------------------------------------

-- | BinaryFunc represents all the binary functions supported
data BinaryFunc = Add | Mul
  deriving (Eq)

-- | declaring custom show instance for BinaryFunc
instance Show BinaryFunc where
  show Add = "+"
  show Mul = "*"

-- | evalBinary associate binary functions to their implementation
evalBinary :: BinaryFunc -> Double -> Double -> Double
evalBinary Add = (+)
evalBinary Mul = (*)

-- | UnaryFunc represents all the unary functions supported
data UnaryFunc = Sin | Cos
  deriving (Eq)

-- | declaring custom show instance for UnaryFunc
instance Show UnaryFunc where
  show Sin = "sin"
  show Cos = "cos"

-- | evalUnary associate unary functions to their implementation
evalUnary :: UnaryFunc -> Double -> Double
evalUnary Sin = Prelude.sin
evalUnary Cos = Prelude.cos

-- | Expr represents the expression data type
data Expr
  = Num Double
  | Var
  | Binary BinaryFunc Expr Expr
  | Unary UnaryFunc Expr
  deriving (Eq)

-- | declaring custom show instance for Expr
instance Show Expr where
  show = showExpr

instance Arbitrary Expr where
  arbitrary = undefined

-- | constructors -------------------------------------------------------------

-- | instantiate a variable
x :: Expr
x = Var

-- | instantiate a number
num :: Double -> Expr
num = Num

-- | instantiate one of the supported binary expressions
add, mul :: Expr -> Expr -> Expr
add = Binary Add
mul = Binary Mul

-- | instantiate one of the supported unary expressions
sin, cos :: Expr -> Expr
sin = Unary Sin
cos = Unary Cos

-- | examples -------------------------------------------------------------
ex1 :: Expr
ex1 = add (mul (num 3) x) (num 5)

ex2 :: Expr
ex2 = add (mul (num 3) (Expr.cos x)) (num 5)

ex3 :: Expr
ex3 = mul (add (num 3) x) (add (num 5) x)

ex4 :: Expr
ex4 = mul (add (num 3) (Expr.sin (add x x))) (add (num 5) x)

-- | utility functions ---------------------------------------------------------

-- | size returns the number of functions and operators in the expression
size :: Expr -> Int
size Var = 0
size (Num _) = 0
size (Binary _ e1 e2) = 1 + size e1 + size e2
size (Unary _ e) = 1 + size e

-- | showExpr returns a string representation of the expression
showExpr :: Expr -> String
showExpr Var = "x"
showExpr (Num n)
  | n == fromInteger (round n) = show (round n :: Integer)
  | otherwise = show n
showExpr (Binary op e1 e2) = wrap e1 ++ " " ++ show op ++ " " ++ wrap e2
  where
    wrap e@(Binary nestedOp _ _)
      | show op == "*" && show nestedOp == "+" =
          "(" ++ showExpr e ++ ")"
    wrap e = showExpr e
showExpr (Unary f e) = show f ++ " " ++ wrap e
  where
    wrap (Binary {}) = "(" ++ showExpr e ++ ")"
    wrap _ = showExpr e

eval :: Expr -> Double -> Double
eval Var c = c
eval (Num n) _ = n
eval (Binary op e1 e2) c = evalBinary op (eval e1 c) (eval e2 c)
eval (Unary f e) c = evalUnary f (eval e c)

readExpr :: String -> Maybe Expr
readExpr = undefined

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr = undefined

arbExpr :: Int -> Gen Expr
arbExpr = undefined

simplify :: Expr -> Expr
simplify = undefined

differentiate :: Expr -> Expr
differentiate = undefined
