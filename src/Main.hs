module Main where

import Data.Char (isSpace)
import Data.List (intercalate)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

import qualified Syntax
import qualified Parse


type Environment = [(String, Syntax.Expression)]

data ReplState = ReplState
    { stateEnvironment :: Environment
    , statePreviousDirective :: String
    , stateDefaultMaxEvaluationSteps :: Int }


helpText :: String
helpText = intercalate "\n"
    [ ":         Run the previous directive"
    , ":?        Display this help text"
    , ":q        Close the REPL"
    , ":d name   Show the definition of name"
    , ":s        Display current definitions" ]


trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

readInput :: IO String
readInput = do
    putStr "> "
    hFlush stdout
    fmap trim getLine

showBinding :: (String, Syntax.Expression) -> String
showBinding (name, expression) =
    printf "%s = %s" name (Syntax.showExpression expression)

showEnvironment :: Environment -> String
showEnvironment = intercalate "\n" . map showBinding

showDefinitionOf :: Environment -> String -> String
showDefinitionOf env name = maybe "Undefined" Syntax.showExpression $ lookup name env

runDirective :: ReplState -> String -> IO ()
runDirective oldState directive =
    let state = oldState { statePreviousDirective = directive }
    in case directive of
        []           -> runDirective oldState (statePreviousDirective oldState)
        "q"          -> pure ()
        "?"          -> putStrLn helpText >> repl state
        "d"          -> putStrLn "Usage: ':d name'" >> repl state
        'd':' ':name -> putStrLn (showDefinitionOf (stateEnvironment state) name) >> repl state
        "s"          -> putStrLn (showEnvironment $ stateEnvironment state) >> repl state
        _            -> putStrLn "Unrecognized REPL directive" >> repl state

repl :: ReplState -> IO ()
repl state = readInput >>= \case
    [] -> repl state
    ':':directive -> runDirective state directive
    string -> case Parse.parseTopLevel string of
        Left message -> putStrLn message >> repl state
        Right topLevel -> case topLevel of
            Syntax.TopLevelDefinition name expression ->
                repl state { stateEnvironment = (name, expression) : stateEnvironment state }
            Syntax.ToplevelExpression expression ->
                putStrLn (Syntax.showExpression expression) >> repl state

main :: IO ()
main = do
    putStrLn "Welcome to lambda-repl! Enter :? for help."
    repl ReplState
        { stateEnvironment               = []
        , statePreviousDirective         = "?"
        , stateDefaultMaxEvaluationSteps = 1000 }
