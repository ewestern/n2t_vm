module Main where

import Parser
import Types
import Code
import Text.Parsec
import System.Environment
import Control.Monad.State

-- todo: get state monad to work so as to supply line numbers (and possible other info) to translate function, so jumps can work correctly.

main = do
  args <- getArgs
  prog <- readFile $ head args
  let path = takeWhile (\x -> x /= '.') (head args)
  case parse parseProgram "vm" prog of 
    Left err -> print err    
    Right xs -> writeFile (path ++ ".asm") $ toAssembly . concat $ translate' xs
      where 
        translate' [] = []
        translate' (y:ys) = do
          ln <- get
          put (ln + 1)
          return (translate y ln):translate' ys
