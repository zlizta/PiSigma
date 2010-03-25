module Main where

import System.Environment

import Tools.Interpreter.REPL

main :: IO ()
main =
  do
    args <- getArgs
    let ini = mapM_ (handleCommand . Load) args
    runREPL (handleCommand Startup >> ini >> repl)

-- TODO: better command line arg handling; allow non-interactive mode

