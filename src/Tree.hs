module Tree
( Expression(..)
, TopLevel(..)
, expressionToString
, topLevelToString
) where

import Text.Printf (printf)


data Expression
    = Application Expression Expression
    | Abstraction String Expression
    | Variable String
    deriving (Show, Eq)

data TopLevel
    = ToplevelExpression Expression
    | TopLevelDefinition String Expression
    deriving (Show, Eq)


expressionToString :: Expression -> String
expressionToString = \case
    Application invocable argument ->
        printf "(%s %s)" (expressionToString invocable) (expressionToString argument)
    Abstraction parameter body ->
        printf "\\%s.%s" parameter (expressionToString body)
    Variable name -> name

topLevelToString :: TopLevel -> String
topLevelToString = \case
    ToplevelExpression expression ->
        expressionToString expression
    TopLevelDefinition name expression ->
        printf "%s = %s" name (expressionToString expression)
