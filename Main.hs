module Main where
import EvalLisp
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if length args > 0
        then loadForRepl (head args)
        else repl