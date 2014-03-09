module Code where
import Types
import Numeric
import Data.Char
--import qualified Data.HashTable.ST.Cuckoo as T
--import Control.Monad.ST
--import Control.Monad.State

numToBin :: Int -> String
numToBin i = replicate (16 - length str) '0' ++ str
  where str = showIntAtBase 2 intToDigit i ""

instance Translatable MemAccess where
  translate (Push (MemLoc Constant i)) = [AInstruction (show i),
                                          CInstruction [D] (Comp (Just A) Nothing Nothing Nothing) Nothing,
                                          AInstruction "SP",                                          
                                          CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing,
                                          movePointer Plus]
  translate _                          = []


--Add | Sub | Neg | Eq | Gt | Lt | And' | Or' | Not'
instance Translatable Arithmetic where
  translate Add    = makeBinaryOp Plus
  translate Sub    = makeBinaryOp Minus
  translate And'   = makeBinaryOp And
  translate Or'    = makeBinaryOp Or

  translate Not'   = makeUnaryOp Not
  translate Neg    = makeUnaryOp Minus

  translate Eq     = makeComparisonOp JEQ
  translate Gt     = makeComparisonOp JGT
  translate Lt     = makeComparisonOp JLT
  --translate Neg = makeBinaryOp

--makeCompOp :: Jump

makeComparisonOp :: Jump -> [Instruction]
makeComparisonOp  jmp =  [AInstruction "SP",
                         movePointer Minus,
                         CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing, -- A = M => M is the value of the topmost item in the stack
                         CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing, -- D = M => D is the topmostvalue in the stack
                         movePointer Minus,
                         CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing, -- A = M => M is the value of the second topmost item in the stack
                         CInstruction [D] (Comp (Just D) (Just Minus) (Just M) Nothing) Nothing,
                         AInstruction "COMP", -- Need way to generate these automagically
                         CInstruction [] (Comp (Just D) Nothing Nothing Nothing) (Just jmp)] ++ -- JUMP!
                         (loadBool False) ++
                         [movePointer Plus,
                         Pseudo "COMP"] ++
                         loadBool True ++
                         [movePointer Plus]

loadBool :: Bool -> [Instruction]
loadBool  b =  [AInstruction $ replicate 16 char,
               CInstruction [D] (Comp (Just A) Nothing Nothing Nothing) Nothing,
               AInstruction "SP",
               CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing]
                where char = if b then '1' else '0'

makeUnaryOp :: Operator -> [Instruction]
makeUnaryOp o =   [AInstruction "SP",
                   movePointer Minus,
                   CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing, -- A = M => M is the value of the topmost item in the stack
                   CInstruction [M] (Comp Nothing (Just o) (Just M) Nothing) Nothing, -- perform op on M => 
                   movePointer Plus]

makeBinaryOp :: Operator -> [Instruction]
makeBinaryOp o =    [AInstruction "SP",-- M is now the address of the stack top.
                     movePointer Minus, -- M = M - 1 => Now M is the address of the topmost item
                     CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing, -- A = M => M is the value of the topmost item in the stack
                     CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing, -- D = M => D is the topmostvalue in the stack
                     movePointer Minus,
                     CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing, -- A = M => M is the value of the second topmost item in the stack
                     CInstruction [M] (Comp (Just D) (Just o) (Just M) Nothing) Nothing, --M = DopM => M is now the value of the computation
-- error expression involves M, but A = -1 is an illegal memory address
                     AInstruction "SP",
                     movePointer Plus] -- Set M to the address of stack top

movePointer :: Operator -> Instruction
movePointer op = CInstruction [A] (Comp (Just A) (Just op) Nothing (Just One)) Nothing

--instance Translatable MemLoc where
--  translate (Memloc s i) = 
--    case s of 
--      --load A with index, move i to D, load A with arg pointer, increment arg pointer by i
--      Argument  ->  pointMTo (MemLoc s i) "ARG"
--      Local     ->  pointMTo (MemLoc s i) "LCL"
--      Static    ->  []
--      Constant  -> [AInstruction show i]
--      This      -> pointMTo (MemLoc s i) "THIS"
--      That      -> pointMTo (MemLoc s i) "THAT"
--      Pointer   -> []
--      Temp      -> []


instance Translatable Command where
  translate (MemAccess' a)  = translate a
  translate (Arithmetic' a) = translate a
--instance Translatable [Command] where
--  translate = 


evalCommand :: Command -> [String]
evalCommand = map toAssembly .  translate  

push = MemAccess' (Push (MemLoc Constant 5))
cinst = CInstruction [D] (Comp (Just D) (Just Plus) (Just M) Nothing) (Just JMP) 
pop = MemAccess' (Pop (MemLoc Local 5))
