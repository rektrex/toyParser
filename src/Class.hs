import Control.Applicative

type Error = String

newtype Parser a = P { getP :: String -> (String, Either Error a) }

instance Functor Parser where
  fmap f (P parser) = P $ \st -> case parser st of
                                   (res, Left err) -> (res, Left err)
                                   (res, Right a) -> (res, Right $ f a)

instance Applicative Parser where
  pure a = P $ \st -> (st, Right a)
  P f <*> P g = P $ \st ->
    case f st of
      (st', Left err) -> (st', Left err)
      (st', Right a) ->
        case g st' of
          (st'', Left err) -> (st'', Left err)
          (st'', Right b) -> (st'', Right $ a b)

instance Alternative Parser where
  empty = P $ \st -> (st, Left "empty")
  (P p1) <|> (P p2) = P $ \st ->
    case p1 st of
      (st', Left err) -> p2 st
      (st', Right val) -> (st', Right val)
