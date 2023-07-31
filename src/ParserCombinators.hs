module ParserCombinators
( Position(..)
, Input(..)
, ParseError(..)
, Parser
) where

import Control.Applicative (Alternative(..))


data Position = Position
    { positionLine   :: Int
    , positionColumn :: Int }
    deriving (Show, Eq, Ord)

data Input = Input
    { inputString   :: String
    , inputPosition :: Position }
    deriving (Show, Eq)

data ParseError
    = ExpectationFailure Position String
    | AlternativeError ParseError ParseError
    deriving Show

newtype Parser a = Parser
    { parse :: Input -> Either ParseError (a, Input) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser \i -> do
        (x, i) <- parse p i
        Right (f x, i)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser \i -> Right (x, i)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = Parser \i -> do
        (f, i) <- parse pf i
        (x, i) <- parse px i
        Right (f x, i)

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Parser \i -> do
        (x, i) <- parse p i
        parse (f x) i

instance Alternative Parser where
    empty :: Parser a
    empty = undefined

    (<|>) :: Parser a -> Parser a -> Parser a
    px <|> py = Parser \i -> case (parse px i, parse py i) of
        (Right (x1, i1), Right (x2, i2)) ->
            if inputPosition i1 >= inputPosition i2
            then Right (x1, i1)
            else Right (x2, i2)
        (Left e1, Left e2) -> Left $ AlternativeError e1 e2
        (Left _, r) -> r
        (l, Left _) -> l
