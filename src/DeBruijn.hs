module DeBruijn
( Index(..)
, Expression(..)
, showExpression
, fromSyntax
) where

import Data.List (elemIndex)
import Text.Printf (printf)

import qualified Syntax (Expression(..))


newtype Index = Index Int
    deriving (Show, Eq)

data Expression
    = Application Expression Expression
    | Abstraction Expression
    | BoundVariable Index
    | UnboundVariable String
    deriving (Show, Eq)


showExpression :: Expression -> String
showExpression = \case
    Application invocable argument ->
        printf "(%s %s)" (showExpression invocable) (showExpression argument)
    Abstraction body ->
        printf "\\%s" (showExpression body)
    BoundVariable (Index index) ->
        show index
    UnboundVariable name ->
        name


fromSyntax :: [String] -> Syntax.Expression -> Expression
fromSyntax variables = \case
    Syntax.Application invocable argument ->
        Application (fromSyntax variables invocable) (fromSyntax variables argument)
    Syntax.Abstraction parameter body ->
        Abstraction (fromSyntax (parameter : variables) body)
    Syntax.Variable name ->
        case elemIndex name variables of
            Just index -> BoundVariable (Index index)
            Nothing    -> UnboundVariable name
