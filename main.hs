module Main where

import Parser
import Types
import Code
import Text.Parsec
import System.Environment


main = do
  args <- getArgs
  prog <- readFile $ head args
  let path = takeWhile (\x -> x /= '.') (head args)
  case parse parseProgram "vm" prog of 
    Left err -> print err    
    Right x -> writeFile (path ++ ".asm") $ toAssembly . concat . fmap translate $ x
