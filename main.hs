module Main where

import Parser
import Types
import Code
import Text.Parsec
import System.Environment


main = do
  args <- getArgs
  prog <- readFile $ head args
  case parse parseProgram "vm" prog of 
    Left err -> print err
    Right x -> print x