module Tree
( Expression(..)
, TopLevel(..)
, showExpression
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


showExpression :: Expression -> String
showExpression = \case
    Application invocable argument ->
        printf "(%s %s)" (showExpression invocable) (showExpression argument)
    Abstraction parameter body ->
        printf "\\%s.%s" parameter (showExpression body)
    Variable name -> name
