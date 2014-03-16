module Main where

import Parser
import Types
import Code
import Text.Parsec hiding (State)
import System.Environment
import Control.Monad.State
import Data.String.Utils

-- todo: get state monad to work so as to supply line numbers (and possible other info) to translate function, so jumps can work correctly.

main = do
  args <- getArgs
  prog <- readFile $ head args
  let path = takeWhile (\x -> x /= '.') (head args)
  let fn = last $ split "/" path
  case parse parseProgram "vm" prog of 
    Left err -> print err    
    Right xs -> writeFile (path ++ ".asm") $ toAssembly . concat $ evalState (translate' xs) (0, fn)
      where 
        translate' :: (Translatable a) => [a] -> State (Int, String) [[Instruction]] 
        translate' [] = return []
        translate' (y:ys) = do
          (n, p) <- get
          put (n + 1, p)
          ys' <- translate' ys
          return $ (translate y (n, p)):ys'

--filterList :: [[Instrction]] []