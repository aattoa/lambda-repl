module ParserCombinators
( Position(..)
, Input(..)
, ParseError(..)
, Parser(..)
, mkInput
, char
, pred
, string
) where

import Prelude hiding (pred)
import Control.Applicative (Alternative(..))
import Data.Foldable (traverse_)
import Text.Printf (printf)


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
        (Left e1, Left e2) -> Left $ coalesceError e1 e2
        (Left _, r) -> r
        (l, Left _) -> l


coalesceError :: ParseError -> ParseError -> ParseError
coalesceError e1@(ExpectationFailure lpos _) e2@(ExpectationFailure rpos _) =
    case compare lpos rpos of
        LT -> e2
        GT -> e1
        EQ -> AlternativeError e1 e2
coalesceError e1 e2 = AlternativeError e1 e2


mkInput :: String -> Input
mkInput = flip Input $ Position 1 1

advanceWith :: Position -> Char -> Position
advanceWith (Position line column) = \case
    '\n' -> Position (succ line) 1
    _    -> Position line (succ column)

summarizeRestOfInput :: String -> String
summarizeRestOfInput = printf "'%s'" . take 10


pred :: String -> (Char -> Bool) -> Parser Char
pred description predicate = Parser \i ->
    let errorFound = Left .
                     ExpectationFailure (inputPosition i) .
                     printf "Expected %s, but found %s" description
    in case inputString i of
    (c:cs) | predicate c -> Right (c, Input cs $ inputPosition i `advanceWith` c)
           | otherwise   -> errorFound $ summarizeRestOfInput $ inputString i
    []                   -> errorFound "the end of input"

char :: Char -> Parser Char
char c = pred ['\'', c, '\''] (==c)

string :: String -> Parser String
string s = s <$ traverse_ char s
