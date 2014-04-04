module Code where
import Types
import Numeric
import Data.Char

bootstrap :: [Instruction]
bootstrap = [AInstruction "256",
             CInstruction [D] (Comp (Just A) Nothing Nothing Nothing) Nothing,
             AInstruction "SP",
             CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing] ++
             translate (Goto "Sys.init") (0, "")
             --translate (Call "Sys.init" 0) (0, "")


numToBin :: Int -> String
numToBin i = replicate (16 - length str) '0' ++ str
  where str = showIntAtBase 2 intToDigit i ""

instance Translatable MemAccess where
  translate (Push (MemLoc Constant i)) n     = [AInstruction (show i),
                                                CInstruction [D] (Comp (Just A) Nothing Nothing Nothing) Nothing,
                                                AInstruction "SP",
                                                CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                                                CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing] ++
                                                movePointer Plus
    
  translate (Push (MemLoc Local i)) n        = translatePushSegments "LCL" i
  translate (Push (MemLoc Argument i)) n     = translatePushSegments "ARG" i
  translate (Push (MemLoc This i)) n         = translatePushSegments "THIS" i
  translate (Push (MemLoc That i)) n         = translatePushSegments "THAT" i
  translate (Pop (MemLoc Local i)) n         = translatePopSegments "LCL" i
  translate (Pop (MemLoc Argument i)) n      = translatePopSegments "ARG" i
  translate (Pop (MemLoc This i)) n          = translatePopSegments "THIS" i
  translate (Pop (MemLoc That i)) n          = translatePopSegments "THAT" i
  translate (Pop (MemLoc Temp i)) n          = translateTempPop 5 i
  translate (Push (MemLoc Temp i)) n         = translateTempPush 5 i
    
  translate (Pop (MemLoc Pointer i)) n       = translateTempPop 3 i
  translate (Push (MemLoc Pointer i)) n      = translateTempPush 3 i 

  translate (Pop (MemLoc Static i)) (n, f)   =  movePointer Minus ++
                                                [CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                                                AInstruction $ f ++ "." ++ show i,
                                                CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing]

  translate (Push (MemLoc Static i)) (n, f)  =  [AInstruction $ f ++ "." ++ show i,
                                                CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                                                AInstruction "SP",
                                                CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                                                CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing] ++
                                                movePointer Plus

  translate _ _                         = []


instance Translatable Flow where
  translate (Label s) n = [Pseudo s]
  translate (Goto s) _ = [AInstruction s,
                          (CInstruction [] (Comp Nothing Nothing Nothing (Just Zero)) (Just JMP))]
  -- pop value from stack. if not zero, jump
  translate (IfGoto s) n = movePointer Minus ++
                          [CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                          AInstruction s,
                          CInstruction [] (Comp (Just D) Nothing Nothing Nothing) (Just JNE)]

instance Translatable FCall where
  translate (Call fname nargs) (n, _) = translateReturnAddressPush ("RETURN" ++ show n) ++
                                        translatePointerPush "LCL" ++
                                        translatePointerPush "ARG" ++
                                        translatePointerPush "THIS" ++
                                        translatePointerPush "THAT" ++
                                  --translate move arg
                                        [AInstruction "SP",
                                        CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                                        AInstruction $ show $ n + 5,
                                        CInstruction [D] (Comp (Just D) (Just Minus) (Just A) Nothing) Nothing,
                                        AInstruction "ARG",
                                        CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing,
--translate move lcl
                                        AInstruction "SP",
                                        CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                                        AInstruction "LCL",
                                        CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing] ++
                                        translate (Goto fname) (0, "") ++ 
                                        [Pseudo $ "RETURN" ++ show n]

  translate (Function fname kargs) (n, _) =  [Pseudo fname] ++ (concat $ take (fromIntegral kargs) $ 
                                              repeat ([AInstruction "SP",
                                              CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                                              CInstruction [M] (Comp Nothing Nothing Nothing (Just Zero)) Nothing] ++
                                              movePointer Plus))


  translate (Return) (n, _)             = [AInstruction "LCL",
                                           CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                                           --frame -> Ln 81
                                           AInstruction "R13",
                                           CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing] ++
                                           --ret variable
                                           (translateFrameToPoint "R14" 5) ++ 
                                           --pop to arg
                                           movePointer Minus ++
                                           [CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                                           AInstruction "ARG",
                                           CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                                           CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing,
                                           --SP = Arg + 1
                                           AInstruction "ARG",
                                           CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                                           AInstruction "SP",
                                           CInstruction [M] (Comp (Just D) (Just Plus) Nothing (Just One)) Nothing] ++
                                           (translateFrameToPoint "THAT" 1) ++
                                           (translateFrameToPoint "THIS" 2) ++
                                           (translateFrameToPoint "ARG" 3) ++
                                           (translateFrameToPoint "LCL" 4) ++
                                           [AInstruction "R14",
                                           CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                                           CInstruction [] (Comp Nothing Nothing Nothing (Just Zero)) (Just JMP)]







translateFrameToPoint :: String -> Integer -> [Instruction]
translateFrameToPoint point delta = [AInstruction $ show delta,
                                     CInstruction [D] (Comp (Just A) Nothing Nothing Nothing) Nothing,
                                     AInstruction "R13",
                                     --CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                                     CInstruction [A] (Comp (Just M) (Just Minus) (Just D) Nothing) Nothing,
                                     CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                                     AInstruction point,
                                     CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing]


translatePointerPush :: String -> [Instruction]
translatePointerPush s = [AInstruction s,
-- for return label, only question is whether value is A or M in next line
                          CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                          AInstruction "SP",
                          CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing,                                          
                          CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing] ++
                          movePointer Plus

translateReturnAddressPush :: String -> [Instruction]
translateReturnAddressPush s = [AInstruction s,
-- for return label, only question is whether value is A or M in next line
                          CInstruction [D] (Comp (Just A) Nothing Nothing Nothing) Nothing,
                          AInstruction "SP",
                          CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing,                                          
                          CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing] ++
                          movePointer Plus


translatePushSegments :: String -> Integer -> [Instruction]
translatePushSegments str i = [AInstruction (show i),
                          CInstruction [D] (Comp (Just A) Nothing Nothing Nothing) Nothing,
                          AInstruction str,
                          CInstruction [A] (Comp (Just M) (Just Plus) (Just D) Nothing) Nothing,
                          CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                          AInstruction "SP",
                          CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                          CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing] ++
                          movePointer Plus

translateTempPush :: Integer -> Integer -> [Instruction]
translateTempPush base i = [AInstruction $ "R" ++ (show $ base + i),
                           CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                           AInstruction "SP",
                           CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                           CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing] ++
                           movePointer Plus


translateTempPop :: Integer -> Integer -> [Instruction]
translateTempPop base i = movePointer Minus ++
                        [CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                        AInstruction $ "R" ++ (show $ base + i),
                        CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing]

translatePopSegments :: String -> Integer -> [Instruction]
-- decrement SP
translatePopSegments str i = [AInstruction (show i), 
                              CInstruction [D] (Comp (Just A) Nothing Nothing Nothing) Nothing, 
                              AInstruction str, 
                              CInstruction [D] (Comp (Just M) (Just Plus) (Just D) Nothing) Nothing,
                              AInstruction "R13",
                              CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing] ++
                              -- address is stashed at R13
                              movePointer Minus ++
                              [CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                              AInstruction "R13",
                              CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing,
                              CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing]
                              
                              
instance Translatable Arithmetic where
  translate Add n      = makeBinaryOp Plus
  translate Sub n      = makeBinaryOp Minus
  translate And' n     = makeBinaryOp And
  translate Or' n      = makeBinaryOp Or

  translate Not' n     = makeUnaryOp Not
  translate Neg  n     = makeUnaryOp Minus

  translate Eq (n, _)  = makeComparisonOp JEQ n
  translate Gt (n, _)  = makeComparisonOp JGT n
  translate Lt (n, _)  = makeComparisonOp JLT n

movePointer :: Operator -> [Instruction]
movePointer op = [AInstruction "SP",
--want to store the incremented address in @SP (M=) as well as have M refer to the value addressed by that incremented address (A=)
                  CInstruction [A,M] (Comp (Just M) (Just op) Nothing (Just One)) Nothing]



makeBinaryOp :: Operator -> [Instruction]
makeBinaryOp o =     movePointer Minus ++ 
                     [CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing] ++
                     movePointer Minus ++
                     [CInstruction [M] (Comp (Just M) (Just o) (Just D) Nothing) Nothing] ++ --M = DopM => M is now the value of the computation
                     movePointer Plus

makeComparisonOp :: Jump -> Int -> [Instruction]
makeComparisonOp jmp n = movePointer Minus ++
                         [CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing] ++
                         movePointer Minus ++
                         [CInstruction [D] (Comp (Just M) (Just Minus) (Just D) Nothing) Nothing,
                         AInstruction $ "COMP"  ++ show n, -- Need way to generate these automagically
                         CInstruction [] (Comp (Just D) Nothing Nothing Nothing) (Just jmp)] ++ -- JUMP!
                         (loadBool False) ++
                         movePointer Plus ++
                         --need to jump over this!!
                         [AInstruction $ "COMP" ++ show (n + 1),
                         CInstruction [] (Comp Nothing Nothing Nothing (Just Zero)) (Just JMP),
                         Pseudo $ "COMP"  ++ show n] ++
                         loadBool True ++
                         movePointer Plus ++
                         [Pseudo $ "COMP" ++ show (n + 1)]

loadBool :: Bool -> [Instruction]
loadBool  b =  [AInstruction "0"] ++
               [if b
                then CInstruction [D] (Comp (Just A) (Just Minus) Nothing (Just One)) Nothing
                else CInstruction [D] (Comp (Just A) Nothing Nothing Nothing) Nothing]++
                [CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing,
               CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing]

--loadBool b = [AInstruction "SP", 
--             CInstruction [A] (Comp (Just M) Nothing Nothing Nothing) Nothing] ++ loadBoolC b
--              where  loadBoolC True = [CInstruction [M] (Comp Nothing (Just Minus) Nothing (Just One)) Nothing]
--                     loadBoolC False= [CInstruction [M] (Comp Nothing Nothing Nothing (Just Zero)) Nothing ]


makeUnaryOp :: Operator -> [Instruction]
makeUnaryOp o =   movePointer Minus ++
                   [CInstruction [M] (Comp Nothing (Just o) (Just M) Nothing) Nothing] ++ -- perform op on M => 
                   movePointer Plus

toFilter :: [[Instruction]]
toFilter = [movePointer Minus ++ movePointer Plus,
            movePointer Plus ++ movePointer Minus]

instance Translatable Command where
  translate (MemAccess' a)  = translate a
  translate (Arithmetic' a) = translate a
  translate (Flow' a)       = translate a
  translate (FCall' a)      = translate a
