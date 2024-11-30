module Expr
  ( Expr (..),
    x,
    num,
    add,
    mul,
    Expr.sin,
    Expr.cos,
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

data BinaryFunc = Add | Mul

instance Show BinaryFunc where
  show Add = "+"
  show Mul = "*"

evalBinary :: BinaryFunc -> Double -> Double -> Double
evalBinary Add = (+)
evalBinary Mul = (*)

data UnaryFunc = Sin | Cos

instance Show UnaryFunc where
  show Sin = "sin"
  show Cos = "cos"

evalUnary :: UnaryFunc -> Double -> Double
evalUnary Sin = Prelude.sin
evalUnary Cos = Prelude.cos

data Expr
  = Num Double
  | Var
  | Binary BinaryFunc Expr Expr
  | Unary UnaryFunc Expr

instance Show Expr where
  show = showExpr

instance Arbitrary Expr where
  arbitrary = undefined

x :: Expr
x = Var

num :: Double -> Expr
num = Num

add, mul :: Expr -> Expr -> Expr
add = Binary Add
mul = Binary Mul

sin, cos :: Expr -> Expr
sin = Unary Sin
cos = Unary Cos

size :: Expr -> Int
size Var = 0
size (Num _) = 0
size (Binary _ e1 e2) = 1 + size e1 + size e2
size (Unary _ e) = 1 + size e

showExpr :: Expr -> String
showExpr Var = "x"
showExpr (Num n) = show n
showExpr (Binary op e1 e2) = wrap e1 ++ show op ++ wrap e2
  where
    wrap (Binary nestedOp _ _)
      | show op == "*" && show nestedOp == "+" =
          "(" ++ showExpr e1 ++ ")"
    wrap _ = showExpr e1
showExpr (Unary f e) = show f ++ wrap e
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
