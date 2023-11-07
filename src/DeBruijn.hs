module DeBruijn
( Index(..)
, Expression(..)
, showExpression
, fromSyntax
, toSyntax
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

makeParameterName :: Int -> String
makeParameterName n = (names !! mod n count) : replicate (div n count) '\'' where
    names = ['a' .. 'z']
    count = length names

fromSyntax :: Syntax.Expression -> Expression
fromSyntax = impl [] where
    impl variables = \case
        Syntax.Application invocable argument ->
            Application (impl variables invocable) (impl variables argument)
        Syntax.Abstraction parameter body ->
            Abstraction (impl (parameter : variables) body)
        Syntax.Variable name ->
            case elemIndex name variables of
                Just index -> BoundVariable (Index index)
                Nothing    -> UnboundVariable name

toSyntax :: Expression -> Syntax.Expression
toSyntax = impl [] where
    impl parameters = \case
        Application invocable argument ->
            Syntax.Application (impl parameters invocable) (impl parameters argument)
        Abstraction body ->
            let parameter = makeParameterName (length parameters)
            in Syntax.Abstraction parameter (impl (parameter : parameters) body)
        BoundVariable (Index index) ->
            Syntax.Variable (parameters !! index)
        UnboundVariable name ->
            Syntax.Variable name
