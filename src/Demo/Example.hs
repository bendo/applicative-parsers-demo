module Demo.Example where

import Control.Applicative

import Data.Bifunctor (second)
import Data.Char (isDigit)
import Data.Functor (($>))

import Demo.Parser

-- | Binary operators (addition and multiplication).
data Operator = Add | Mul deriving (Eq, Show)

-- | Simple arithmetic expression AST.
data Expr = ConstExpr  Int                -- ^ Positive integer constant.
          | BinaryExpr Expr Operator Expr -- ^ Binary operator application.
          | NegateExpr Expr               -- ^ Negation (unary operator).
          deriving (Eq, Show)

-- | Parse arithmetic expressions, with the following grammar:
--   expr      ::= constExpr | binOpExpr | negExpr
--   const     ::= int
--   int       ::= digit{digit}
--   digit     ::= '0' | ... | '9'
--   binOpExpr ::= '(' expr ' ' binOp ' ' expr ')'
--   binOp     ::= '+' | '*'
--   negExpr   ::= '-' expr
exprParser :: Parser Expr
exprParser = constParser <|> binParser <|> negParser

-- | Constant expression parser.
--   const ::= int
constParser :: Parser Expr
constParser = ConstExpr <$> intParser

-- | Positive integer parser.
--   int   ::= digit{digit}
--   digit ::= '0' | ... | '9'
intParser :: Parser Int
intParser = Parser $ \s -> let res = unParser (some digitParser) s in
  case res of
      [] -> []
      (x : xs) -> [second read x]
  where
    digitParser = predP isDigit

-- | Binary operator expression parser.
--   binOpExpr ::= '(' expr ' ' binOp ' ' expr ')'
binParser :: Parser Expr
binParser =
  charP '(' *>
    (BinaryExpr <$> exprParser
                <*> (charP ' ' *> binOpParser <* charP ' ')
                <*> exprParser
    )
  <* charP ')'


-- | Binary operator symbol parser.
--   binOp ::= '+' | '*'
binOpParser :: Parser Operator
binOpParser = plusParser <|> multParser
  where
    plusParser = charP '+' $> Add
    multParser = charP '*' $> Mul

-- | Negation parser.
--   negExpr ::= '-' expr
negParser = charP '-' *> (NegateExpr <$> exprParser)
