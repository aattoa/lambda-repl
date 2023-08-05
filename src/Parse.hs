module Parse
( parseExpression
, parseTopLevel
) where

import Control.Applicative ((<|>), some, many)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (foldl')
import Text.Printf (printf)

import qualified ParserCombinators as PC
import qualified Tree


makeChurchNumeral :: Integer -> Tree.Expression
makeChurchNumeral = Tree.Abstraction "f" . Tree.Abstraction "x" . helper
    where helper 0 = Tree.Variable "x"
          helper n = Tree.Application (Tree.Variable "f") (helper $ pred n)

identifier :: PC.Parser String
identifier = (:)
    <$> PC.pred "an identifier" isAlpha
    <*> many (PC.pred "" (\c -> isAlphaNum c || c `elem` "_'"))

ws :: PC.Parser ()
ws = void $ many $ PC.pred "" isSpace

wsd :: PC.Parser a -> PC.Parser a
wsd p = ws *> p <* ws

paren :: PC.Parser a -> PC.Parser a
paren p = wsd (PC.char '(') *> p <* wsd (PC.char ')')

abstraction :: PC.Parser Tree.Expression
abstraction = flip (foldr Tree.Abstraction)
    <$> (wsd (PC.char '\\') *> some (wsd identifier))
    <*> (wsd (PC.char '.') *> expression)

variable :: PC.Parser Tree.Expression
variable = Tree.Variable <$> wsd identifier

numeral :: PC.Parser Tree.Expression
numeral = makeChurchNumeral . read <$> some (PC.pred "a digit" isDigit)

expression :: PC.Parser Tree.Expression
expression = foldl' Tree.Application <$> expr <*> many expr
    where expr = variable <|> abstraction <|> numeral <|> paren expression

definition :: PC.Parser Tree.TopLevel
definition = Tree.TopLevelDefinition
    <$> (wsd identifier <* wsd (PC.char '='))
    <*> expression

topLevel :: PC.Parser Tree.TopLevel
topLevel = definition <|> (Tree.ToplevelExpression <$> expression)

parseErrorToString :: PC.ParseError -> String
parseErrorToString = \case
    PC.AlternativeError l r ->
        printf "((%s) or (%s))" (parseErrorToString l) (parseErrorToString r)
    PC.ExpectationFailure position message ->
        printf "On line %i, column %i: %s" (PC.positionLine position) (PC.positionColumn position) message

doParse :: PC.Parser a -> String -> Either String a
doParse p s = case PC.parse p (PC.mkInput s) of
    Left e -> Left $ parseErrorToString e
    Right (x, i) ->
        if null $ PC.inputString i
        then Right x
        else Left $ printf "Remaining input: '%s'" $ PC.inputString i

parseExpression :: String -> Either String Tree.Expression
parseExpression = doParse expression

parseTopLevel :: String -> Either String Tree.TopLevel
parseTopLevel = doParse topLevel
