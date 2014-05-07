module Main where

import Parser
import Types
import Code
import Text.Parsec hiding (State)
import System.Environment
import Control.Monad.State
import Data.String.Utils
import System.Directory


main = do
  args <- getArgs
  let path = takeWhile (\x -> x /= '.') (head args)
  --print (last $ split "/" path) 
  let fn = last $ split "/" path
  prog <- case fn of
               "" -> do
                    files <- getDirectoryContents path
                    let vmfiles = filter (\f -> (take 3 (reverse f)) == "mv.") files
                    strings <- mapM (readFile . (path ++)) vmfiles
                    return $ concat strings
               _ -> readFile $ head args

  case parse parseProgram "vm" prog of 
    Left err -> print err
    --Right xs -> print xs    
    Right xs -> writeFile (if fn == "" then path ++ (last $ init $ split "/" path) ++ ".asm" else path ++ ".asm") $ translation
      where 
        --translation = toAssembly $ concat $ (evalState (translate' xs) (0, fn))
        translation = toAssembly $ concat $ [bootstrap] ++ (evalState (translate' xs) (0, fn))
        --translation = assemble $ [bootstrap] ++ (evalState (translate' xs) (0, fn))
        --assemble lol = map () (intercalate "\n") . (fmap toAssembly)
        translate' :: (Translatable a) => [a] -> State (Int, String) [[Instruction]] 
        translate' [] = return []
        translate' (y:ys) = do
          (n, p) <- get
          put (n + 2, p)
          ys' <- translate' ys
          return $ (translate y (n, p)):ys'
