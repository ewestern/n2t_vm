module Code where
import Types
import qualified Data.HashTable.ST.Cuckoo as T
import Control.Monad.ST
import Control.Monad.State



instance Translatable MemAccess where
  translate (Push (MemLoc s i)) = [AInstruction (show $ ((segmentAddress s) + i::Int)) ,
                                CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing , 
                                AInstruction "SP", 
                                CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing, 
                                CInstruction [A] (Comp (Just A) (Just Plus) Nothing (Just One)) Nothing]
                                --
  translate (Pop (MemLoc s i)) = [AInstruction "SP",
                                  CInstruction [D] (Comp (Just M) Nothing Nothing Nothing) Nothing,                                   
                                  CInstruction [A] (Comp (Just A) (Just Minus) Nothing (Just One)) Nothing,
                                  AInstruction (show $ ((segmentAddress s) + i::Int)),
                                  CInstruction [M] (Comp (Just D) Nothing Nothing Nothing) Nothing]

instance Translatable Arithmetic where
  translate = []


-- need a structure, essentially, to keep track of the stack pointer
-- This should be doable with the state monad alone
--evalCommand :: Command -> Int -> ()
--evalCommand c sp = case c of
--  MemAccess'(Push (seg i))  ->
--   -- pushing puts (seg i) in sp


--  MemAccess'(Pop (seg i))   -> 
--  Arithmetic' Add           ->  

--memPush a 

--evalProcedure :: Procedure 
--evalProcedure (c:cs) = do
--  ht <- T.new
  
--  --T.insert ht "1" "1"
--  return ht


  --Add, for the vm, will do
segmentAddress :: Segment -> Int
segmentAddress s = case s of
                    Argument  -> 0
                    Local     -> 0
                    Static    -> 0
                    Constant  -> 0
                    This      -> 0
                    That      -> 0
                    Pointer   -> 0
                    Temp      -> 0

