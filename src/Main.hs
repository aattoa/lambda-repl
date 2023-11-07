module Main where

import Data.Bifunctor (second)
import Data.Char (isSpace)
import Data.List (intercalate)
import Text.Printf (printf)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)

import qualified Syntax
import qualified DeBruijn
import qualified Parse


type Environment = [(String, Syntax.Expression)]

data ReplState = ReplState
    { stateEnvironment               :: Environment
    , statePreviousDirective         :: String
    , stateDefaultMaxEvaluationSteps :: Int }


helpText :: String
helpText =
    ":         Run the previous directive  \n\
    \:?        Display this help text      \n\
    \:q        Close the REPL              \n\
    \:d name   Show the definition of name \n\
    \:s        Display current definitions"

mapSecond :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSecond = map . Data.Bifunctor.second

showBinding :: (String, Syntax.Expression) -> String
showBinding (name, expression) =
    printf "%s = %s" name (Syntax.showExpression expression)

showEnvironment :: Environment -> String
showEnvironment = intercalate "\n" . map showBinding

showDefinitionOf :: Environment -> String -> String
showDefinitionOf env name = maybe "Undefined" Syntax.showExpression (lookup name env)

runDirective :: ReplState -> String -> InputT IO ()
runDirective oldState directive =
    let state = oldState { statePreviousDirective = directive }
    in case directive of
        []           -> runDirective oldState (statePreviousDirective oldState)
        "q"          -> outputStrLn "Leaving lambda-repl. Goodbye!"
        "?"          -> outputStrLn helpText >> repl state
        "d"          -> outputStrLn "Usage: ':d name'" >> repl state
        'd':' ':name -> outputStrLn (showDefinitionOf (stateEnvironment state) name) >> repl state
        "s"          -> outputStrLn (showEnvironment $ stateEnvironment state) >> repl state
        _            -> outputStrLn "Unrecognized REPL directive" >> repl oldState

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

readInput :: InputT IO String
readInput = maybe ":q" trim <$> getInputLine "> "

repl :: ReplState -> InputT IO ()
repl state = readInput >>= \case
    [] -> repl state
    ':':directive -> runDirective state directive
    string -> case Parse.parseTopLevel string of
        Left message -> outputStrLn message >> repl state
        Right topLevel ->
            let env = stateEnvironment state
            in case topLevel of
                Syntax.TopLevelDefinition name expression ->
                    repl state { stateEnvironment = (name, expression) : env }
                Syntax.ToplevelExpression expression ->
                    outputStrLn (Syntax.showExpression expression) >> repl state

defaultReplState :: ReplState
defaultReplState =
    ReplState
    { stateEnvironment               = []
    , statePreviousDirective         = "?"
    , stateDefaultMaxEvaluationSteps = 1000
    }

main :: IO ()
main = do
    putStrLn "Welcome to lambda-repl! Enter :? for help."
    runInputT defaultSettings (repl defaultReplState)
