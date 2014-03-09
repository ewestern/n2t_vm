module Code where
import Types
import Numeric
import Data.Char


numToBin :: Int -> String
numToBin i = replicate (16 - length str) '0' ++ str
  where str = showIntAtBase 2 intToDigit i ""

instance Translatable MemAccess where
  translate (Push (MemLoc Constant i)) n = [AInstruction (show i),
                                            CInstruction [D] (Comp (Just A) Nothing Nothing Nothing) Nothing,
                                            AInstruction "SP",
                                            CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing,                                          
                                            CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing,
                                            AInstruction "SP",
                                            movePointer Plus]

  translate _ _                         = []


instance Translatable Arithmetic where
  translate Add n   = makeBinaryOp Plus
  translate Sub n   = makeBinaryOp Minus
  translate And' n  = makeBinaryOp And
  translate Or' n   = makeBinaryOp Or

  translate Not' n  = makeUnaryOp Not
  translate Neg  n  = makeUnaryOp Minus

  translate Eq   n  = makeComparisonOp JEQ n
  translate Gt   n  = makeComparisonOp JGT n
  translate Lt   n  = makeComparisonOp JLT n
  --translate Neg = makeBinaryOp

initStack :: [Instruction]
initStack = [AInstruction "256",
             CInstruction [D] (Comp (Just A) Nothing Nothing Nothing) Nothing,
             AInstruction "SP",
             CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing]

movePointer :: Operator -> Instruction
-- decrement the address @ SP
-- store the decremented address @ SP
-- set A to the SP address
-- now M will refer to the value at that address
movePointer op = CInstruction [A,M] (Comp (Just M) (Just op) Nothing (Just One)) Nothing


makeBinaryOp :: Operator -> [Instruction]
makeBinaryOp o =    [AInstruction "SP",
                     movePointer Minus, 
                     CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                     AInstruction "SP",
                     movePointer Minus,
                     CInstruction [M] (Comp (Just D) (Just o) (Just M) Nothing) Nothing, --M = DopM => M is now the value of the computation
                     AInstruction "SP",
                     movePointer Plus]

makeComparisonOp :: Jump -> Int -> [Instruction]
makeComparisonOp jmp n = [AInstruction "SP",
                         movePointer Minus,
                         CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                         AInstruction "SP",
                         movePointer Minus,
                         CInstruction [D] (Comp (Just D) (Just Minus) (Just M) Nothing) Nothing,
                         AInstruction $ "COMP"  ++ show n, -- Need way to generate these automagically
                         CInstruction [] (Comp (Just D) Nothing Nothing Nothing) (Just jmp)] ++ -- JUMP!
                         (loadBool False) ++
                         [movePointer Plus,
                         Pseudo $ "COMP"  ++ show n] ++
                         loadBool True ++
                         [movePointer Plus]

loadBool :: Bool -> [Instruction]
loadBool  b =  [AInstruction $ replicate 16 char,
               CInstruction [D] (Comp (Just A) Nothing Nothing Nothing) Nothing,
               AInstruction "SP",
               CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing,
               CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing]
                where char = if b then '1' else '0'

makeUnaryOp :: Operator -> [Instruction]
makeUnaryOp o =   [AInstruction "SP",
                   movePointer Minus,
                   CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing, -- A = M => M is the value of the topmost item in the stack
                   CInstruction [M] (Comp Nothing (Just o) (Just M) Nothing) Nothing, -- perform op on M => 
                   movePointer Plus]


instance Translatable Command where
  translate (MemAccess' a)  = translate a
  translate (Arithmetic' a) = translate a


--push = MemAccess' (Push (MemLoc Constant 5))
--cinst = CInstruction [D] (Comp (Just D) (Just Plus) (Just M) Nothing) (Just JMP) 
--pop = MemAccess' (Pop (MemLoc Local 5))
