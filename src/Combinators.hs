module Combinators where

import Class
import Control.Applicative

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P $ \st ->
  case st of
    []                 -> ([], Left "end of stream")
    (c:cs) | f c       -> (cs, Right c)
           | otherwise -> (cs, Left $ "unexpected " ++ show c)

try :: Parser a -> Parser a
try (P p) = P $ \st ->
  case p st of
    (_, Left err)  -> (st, Left err)
    (st', Right a) -> (st', Right a)

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string [] = pure []
string (c:cs) = (:) <$> char c <*> string cs

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)

choice :: Alternative m => [m a] -> m a
choice = foldr (<|>) empty

option :: Alternative m => a -> m a -> m a
option x p = p <|> pure x

skipOptional :: Alternative m => m a -> m ()
skipOptional p = (() <$ p) <|> pure ()

between :: Applicative m => m start -> m end -> m a -> m a
between s e p = s *> p <* e

sepBy1 :: Alternative m => m a -> m sep -> m [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

sepBy :: Alternative m => m a -> m sep -> m [a]
sepBy p sep = sepBy1 p sep <|> pure []

count :: Applicative m => Int -> m a -> m [a]
count n p | n <= 0 = pure []
          | otherwise = sequenceA (replicate n p)
