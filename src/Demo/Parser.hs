module Demo.Parser where

import Control.Applicative

import Data.Bifunctor(second)
import Data.List (isPrefixOf)

newtype Parser a = Parser { unParser :: String -> [(String, a)] }

instance Functor Parser where
  fmap f (Parser p) = Parser (\b -> map (second f) (p b))

instance Applicative Parser where
  pure x = Parser (\s -> [(s, x)])
  pf <*> px = Parser (\s -> [ (sx, f x) | (sf, f) <- unParser pf $ s,
                                          (sx, x) <- unParser px $ sf])

instance Alternative Parser where
  empty = Parser (const [])
  px <|> py = Parser (\s -> unParser px s ++ unParser py s)

parseString :: String -> Parser a -> Maybe a
parseString s (Parser p) = case p s of
    [("", val)] -> Just val
    _           -> Nothing

predP :: (Char -> Bool) -> Parser Char
predP p = Parser f
  where
    f "" = []
    f (c : cs) | p c = [(cs, c)]
               | otherwise = []

charP :: Char -> Parser Char
charP = predP . (==)

stringP :: String -> Parser String
stringP s = Parser f
  where
    f s' | s == s' = [("", s)]
         | otherwise = []

skip :: (Char -> Bool) -> Parser ()
skip p = Parser (\s -> [(dropWhile p s, ())])

prefixP :: String -> Parser String
prefixP s = Parser f
  where
    f input = if s `isPrefixOf` input
                then [(drop (length s) input, s)]
                else []

skipString :: String -> Parser ()
skipString s = () <$ prefixP s

