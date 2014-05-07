{-#LANGUAGE GADTs #-}
{-#LANGUAGE FlexibleInstances #-}

module Types where
import Text.Parsec
import Text.Parsec.String
import Data.String.Utils
import Data.Maybe
import Data.Monoid
import Data.List

type Procedure = [Command]

class Translatable a where
  translate :: a -> (Int, String) -> [Instruction]

data Command where
  Arithmetic' :: Arithmetic -> Command 
  MemAccess' :: MemAccess -> Command
  Flow' :: Flow -> Command
  FCall' :: FCall -> Command
  deriving (Show)
  

data Arithmetic = Add | Sub | Neg | Eq | Gt | Lt | And' | Or' | Not' deriving (Show)

data MemAccess = Pop MemLoc | Push MemLoc deriving (Show)

data Segment = Argument | Local | Static | Constant | This | That | Pointer | Temp deriving (Show)

data Flow = Label String | Goto String | IfGoto String deriving (Show) 

data FCall = Function String Integer | Call String Integer | Return deriving (Show)

data MemLoc = MemLoc {
  name :: Segment,
  index :: Integer
} deriving (Show)

-- ASSEMBLY TYPES
class Assemblable a where
  toAssembly :: a -> String

data Operator = Plus | Minus | Or | And | Not deriving (Eq, Ord, Show)
data Register = A | M | D  deriving (Show, Eq, Ord)
data Bin = Zero | One deriving (Eq, Ord, Show)
data Jump = JGT | JEQ | JGE | JLT | JNE | JLE | JMP deriving (Show, Eq, Ord)
data Comp = Comp {
  reg1 :: Maybe Register,
  op :: Maybe Operator,
  reg2 :: Maybe Register,
  bin :: Maybe Bin
} deriving (Eq, Ord, Show)


instance Assemblable Comp where
  toAssembly (Comp r o r2 b) = mshow r ++ mshow o ++ mshow r2 ++ mshow b
    where mshow c = case c of
                      Just c -> toAssembly c
                      Nothing -> "" 

instance Assemblable Operator where
  toAssembly o = case o of
                  Plus  -> "+"
                  Minus -> "-"
                  Or    -> "|"
                  And   -> "&"
                  Not   -> "!"

instance Assemblable Bin where
  toAssembly b = case b of
                  Zero  -> "0"
                  One   -> "1"

instance Assemblable Register where
  toAssembly = show

instance Assemblable [Instruction] where
  toAssembly = (intercalate "\n") . (fmap toAssembly)

--instance

data Instruction = AInstruction String | CInstruction  {
    dest :: [Register],
    comp :: Comp,
    jump :: Maybe Jump
} | Pseudo String deriving (Show)

--instance Assemblable Register where
--  toAssembly = show

instance Assemblable Instruction where
  toAssembly (AInstruction s) = "@" ++ s
  --toAssembly (AInstruction a) = "@" ++ show i
  toAssembly (Pseudo s) = "(" ++ s ++ ")"
  toAssembly (CInstruction d c j) = dshow ++ toAssembly c ++ jshow
    where dshow = (join "" $ map show d) ++ if d == [] then "" else "="
          jshow = case j of
            Just s -> ";" ++ show s
            Nothing -> ""

