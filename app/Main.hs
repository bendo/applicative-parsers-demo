module Main where

import Demo.Example
import Demo.Parser (parseString)

import System.IO

main :: IO ()
main = do
  putStrLn "Enter expressions to parse:"
  putStrLn "expr      ::= constExpr | binOpExpr | negExpr"
  putStrLn "const     ::= int"
  putStrLn "int       ::= digit{digit}"
  putStrLn "digit     ::= '0' | ... | '9'"
  putStrLn "binOpExpr ::= '(' expr ' ' binOp ' ' expr ')'"
  putStrLn "binOp     ::= '+' | '*'"
  putStrLn "negExpr   ::= '-' expr"
  go
  where
    go = do
      hSetBuffering stdout NoBuffering -- for putStr to print at once
      putStr ">>> "
      str <- getLine
      case parseString str exprParser of
        Just e -> putStrLn ("Ok: " ++ show e)
        _      -> putStrLn "Parse error"
      go
