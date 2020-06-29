module Combinators where

import Class

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
