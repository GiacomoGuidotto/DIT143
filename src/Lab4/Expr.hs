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
    -- parsing
    readExpr,
    -- expression manipulation
    simplify,
    differentiate,
    -- testing
    arbExpr,
    prop_ShowReadExpr,
  )
where

import Data.Char (isSpace)
import Data.Functor
import Parsing
import Test.QuickCheck

-- | data definitions ---------------------------------------------------------

-- BinaryFunc represents all the binary functions supported
data BinaryFunc = Add | Mul
  deriving (Eq)

-- UnaryFunc represents all the unary functions supported
data UnaryFunc = Sin | Cos
  deriving (Eq)

-- Expr represents the expression data type
data Expr
  = Num Double
  | Var
  | Binary BinaryFunc Expr Expr
  | Unary UnaryFunc Expr
  deriving (Eq)

-- | constructors -------------------------------------------------------------

-- instantiate a variable
x :: Expr
x = Var

-- instantiate a number
num :: Double -> Expr
num = Num

-- instantiate one of the supported binary expressions
add, mul :: Expr -> Expr -> Expr
add = Binary Add
mul = Binary Mul

-- instantiate one of the supported unary expressions
sin, cos :: Expr -> Expr
sin = Unary Sin
cos = Unary Cos

-- | mathematical interface ---------------------------------------------------

-- declaring custom show instance for UnaryFunc
instance Show UnaryFunc where
  show Sin = "sin"
  show Cos = "cos"

-- evalUnary associate unary functions to their implementation
evalUnary :: UnaryFunc -> Double -> Double
evalUnary Sin = Prelude.sin
evalUnary Cos = Prelude.cos

-- unaryOps map strings to their corresponding unary function constructors
unaryOps :: [(String, UnaryFunc)]
unaryOps =
  [ ("sin", Sin),
    ("cos", Cos)
  ]

-- declaring custom show instance for BinaryFunc
instance Show BinaryFunc where
  show Add = "+"
  show Mul = "*"

-- evalBinary associate binary functions to their implementation
evalBinary :: BinaryFunc -> Double -> Double -> Double
evalBinary Add = (+)
evalBinary Mul = (*)

binaryPrecedence :: BinaryFunc -> Int
binaryPrecedence Add = 1
binaryPrecedence Mul = 2

-- examples -------------------------------------------------------------------
ex1 :: Expr
ex1 = add (mul (num 3) x) (num 5)

ex2 :: Expr
ex2 = add (mul (num 3) (Expr.cos x)) (num 5)

ex3 :: Expr
ex3 = mul (add (num 3) x) (add (num 5) x)

ex4 :: Expr
ex4 = mul (add (num 3) (Expr.sin (add x x))) (add (num 5) x)

-- | utility functions --------------------------------------------------------

-- size returns the number of functions and operators in the expression
size :: Expr -> Int
size Var = 0
size (Num _) = 0
size (Binary _ e1 e2) = 1 + size e1 + size e2
size (Unary _ e) = 1 + size e

-- showExpr returns a string representation of the expression
showExpr :: Expr -> String
showExpr Var = "x"
showExpr (Num n)
  | n == fromInteger (round n) = show (round n :: Integer)
  | otherwise = show n
showExpr (Binary op e1 e2) = wrap e1 ++ " " ++ show op ++ " " ++ wrap e2
  where
    wrap e@(Binary nestedOp _ _)
      | binaryPrecedence nestedOp < binaryPrecedence op =
          "(" ++ showExpr e ++ ")"
    wrap e = showExpr e
showExpr (Unary f e) = show f ++ " " ++ wrap e
  where
    wrap (Binary {}) = "(" ++ showExpr e ++ ")"
    wrap _ = showExpr e

-- declaring custom show instance for Expr
instance Show Expr where
  show = showExpr

eval :: Expr -> Double -> Double
eval Var c = c
eval (Num n) _ = n
eval (Binary op e1 e2) c = evalBinary op (eval e1 c) (eval e2 c)
eval (Unary f e) c = evalUnary f (eval e c)

-- flatten a nested binary expression of a given operator into a list of subexpressions
flatten :: BinaryFunc -> Expr -> [Expr]
flatten op (Binary op' e1 e2) | op' == op = flatten op e1 ++ flatten op e2
flatten _ e = [e]

normalize :: Expr -> Expr
normalize (Binary op e1 e2) =
  let es = map normalize (flatten op (Binary op e1 e2))
   in foldl1 (Binary op) es
normalize (Unary f e) = Unary f (normalize e)
normalize (Num n) = Num n
normalize Var = Var

-- | parsing ------------------------------------------------------------------

{-
EBNF for the expression language:

space ::= " "
spaces ::= space {space}
digit ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

number ::= spaces, digit {digit}, spaces
var ::= spaces, "x", spaces
unary ::= spaces, "sin" | "cos", spaces

factor ::= number | var | (unary, factor) | "(", expr, ")"

term ::= factor {"*", factor}
expr ::= term {"+", term}
-}

-- | utility functions

-- string parses the input to match a given string
string :: String -> Parser String
string "" = return ""
string (c : cs) = char c <:> string cs

-- space parses zero or more spaces
spaces :: Parser String
spaces = zeroOrMore (sat isSpace)

-- token wrap a parser with space parsers to ignore leading and trailing spaces
token :: Parser a -> Parser a
token p = spaces *> p <* spaces

-- symbol parses a string and ignores leading and trailing spaces
symbol :: String -> Parser String
symbol = token . string

-- parens wrap a parser with "(" or ")" parsers to ignore parentesis
parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

-- foldChain parses a chain of elements separated by a separator `s`
-- and folds them with a given function `f`
foldChain :: Parser a -> Parser b -> (a -> a -> a) -> Parser a
foldChain p s f = do
  es <- chain p s
  case es of
    [] -> failure
    (e : es') -> return $ foldl f e es'

-- | expression parsers

-- varP creates a parser for the string "x" and construct
varP :: Parser Expr
varP = symbol "x" $> Var

-- numberP creates a parser for possibly signed, possibly decimal numbers.
numberP :: Parser Expr
numberP = token ((readsP :: Parser Double) <&> Num)

-- unaryP creates a parser for unary functions names by try to match
-- all the strings in unaryOps and construct a parser for UnaryFunc
unaryP :: Parser UnaryFunc
unaryP = foldr ((<|>) . makeParser) failure unaryOps
  where
    makeParser (s, f) = token (string s $> f)

-- factorP creates a parser for the building blocks of the expression
-- treated as factors of multiplictions. It tries to match unary functions,
-- numbers, variables and parenthesized expressions
factorP :: Parser Expr
factorP =
  (do f <- unaryP; Unary f <$> factorP)
    <|> numberP
    <|> varP
    <|> parens exprP

-- termP creates a parser for multiplcations of factors treated as terms
-- of additions. It uses foldChain to parse a chain of factors separated by
-- the "*" symbol and construct a parser for Expr using the mul constructor
termP :: Parser Expr
termP = foldChain factorP (symbol "*") mul

-- exprP creates a parser for additions of terms. It uses foldChain to parse
-- a chain of terms separated by the "+" symbol and construct a parser for Expr
-- using the add constructor
exprP :: Parser Expr
exprP = foldChain termP (symbol "+") add

-- | parse function

-- readExpr parses a string using the exprP parser and returns the parsed
-- expression if the parsing is successful, otherwise it returns Nothing
readExpr :: String -> Maybe Expr
readExpr s = case parse exprP s of
  Just (e, "") -> Just e
  _ -> Nothing

-- | expression manipulation --------------------------------------------------
simplify :: Expr -> Expr
simplify = undefined

differentiate :: Expr -> Expr
differentiate = undefined

-- | testing ------------------------------------------------------------------

-- | generators
instance Arbitrary UnaryFunc where
  arbitrary = elements [Sin, Cos]

instance Arbitrary BinaryFunc where
  arbitrary = elements [Add, Mul]

arbExpr :: Int -> Gen Expr
arbExpr 0 =
  oneof
    [ return Var,
      Num <$> arbitrary
    ]
arbExpr n =
  oneof
    [ return Var,
      Num <$> arbitrary,
      Binary <$> arbitrary <*> subExpr <*> subExpr,
      Unary <$> arbitrary <*> subExpr
    ]
  where
    subExpr = arbExpr (n `div` 2)

instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- | props

-- prop_ShowReadExpr tests the showExpr and readExpr functions
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = case readExpr (showExpr e) of
  Just e' -> normalize e == normalize e'
  Nothing -> False
