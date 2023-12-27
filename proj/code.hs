-- PFL 2023/24 - Haskell practical assignment quickstart

import Data.List
import qualified Data.Map.Strict as HashMap
import Data.Map (toList)
import Language.Haskell.TH (Lit(IntegerL))

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackValue = StackInt Integer | StackT | StackF deriving (Show, Eq)
type Stack = [StackValue]

-- createEmptyStack :: Stack
createEmptyStack :: Stack
createEmptyStack = []

stackValueToString :: StackValue -> String
stackValueToString (StackInt n) = show n
stackValueToString StackT = "True"
stackValueToString StackF = "False"

-- stack2Str :: Stack -> String
stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map stackValueToString stack)

-- Add the following definition for State
type Key = String
type Val = StackValue

type State = HashMap.Map Key Val

-- createEmptyState :: State
createEmptyState :: State
createEmptyState = HashMap.empty

insertIntoState :: Key -> Val -> State -> State
insertIntoState = HashMap.insert

-- state2Str :: State -> String
state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ stackValueToString val | (var, val) <- toList state]

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)

run (instruction:remainingCode, stack, state) = case instruction of
    Push n -> run (remainingCode, StackInt n:stack, state)
    Add -> runAddBinaryOperation
    Mult -> runMultBinaryOperation
    Sub -> runSubBinaryOperation
    Tru -> run (remainingCode, StackT:stack, state)
    Fals -> run (remainingCode, StackF:stack, state)
    Equ -> runEquComparisonOperation
    Le -> runLeComparisonOperation
    And -> runLogicalOperation (&&)
    Neg -> runNegationOperation
    Fetch key -> runFetchOperation key
    Store key -> runStoreOperation key
    Branch code1 code2 -> runBranchOperation code1 code2
    Loop code1 code2 -> runLoopOperation code1 code2
    Noop -> run (remainingCode, stack, state)
  where
    runAddBinaryOperation = case stack of
        StackInt value1:StackInt value2:restStack -> run (remainingCode, StackInt (value1 + value2):restStack, state)
        _ -> error "Runtime error: Invalid operation"

    runSubBinaryOperation = case stack of
        StackInt value1:StackInt value2:restStack -> run (remainingCode, StackInt (value1 - value2):restStack, state)
        _ -> error "Runtime error: Invalid operation"

    runMultBinaryOperation = case stack of
        StackInt value1:StackInt value2:restStack -> run (remainingCode, StackInt (value1 * value2):restStack, state)
        _ -> error "Runtime error: Invalid operation"

    runEquComparisonOperation = case stack of
        StackInt value1 : StackInt value2 : restStack -> run (remainingCode, value:restStack, state)
          where value = if value1 == value2 then StackT else StackF
        StackT : StackT : restStack -> run (remainingCode, value:restStack, state)
            where value = if StackT == StackT then StackT else StackF
        StackT : StackF : restStack -> run (remainingCode, value:restStack, state)
            where value = if StackT == StackF then StackT else StackF
        StackF : StackT : restStack -> run (remainingCode, value:restStack, state)
            where value = if StackF == StackT then StackT else StackF
        StackF : StackF : restStack -> run (remainingCode, value:restStack, state)
            where value = if StackF == StackF then StackT else StackF
        _ -> error "Runtime error: Invalid operation"

    runLeComparisonOperation = case stack of
            StackInt value1:StackInt value2:restStack -> run (remainingCode, value:restStack, state)
              where value = if value1 <= value2 then StackT else StackF
            _ -> error "Runtime error: Invalid operation"

    runLogicalOperation logOp = case stack of
        StackT:StackT:restStack -> run (remainingCode, StackT:restStack, state)
        StackT:StackF:restStack -> run (remainingCode, StackF:restStack, state)
        StackF:StackT:restStack -> run (remainingCode, StackF:restStack, state)
        StackF:StackF:restStack -> run (remainingCode, StackF:restStack, state)
        StackInt _:restStack -> error "Runtime error: Invalid operation"
        _ -> error "Runtime error: Insufficient operands"

    runNegationOperation = case stack of
        StackT:restStack -> run (remainingCode, StackF:restStack, state)
        StackF:restStack -> run (remainingCode, StackT:restStack, state)
        StackInt _:restStack -> error "Runtime error: Invalid operation"
        _ -> error "Runtime error: Insufficient operands"

    runFetchOperation key = case HashMap.lookup key state of
        Just value -> run (remainingCode, value:stack, state)
        Nothing -> error "Runtime error: Variable not found"

    runStoreOperation key = case stack of
        stacktop:restStack -> run (remainingCode, restStack, insertIntoState key stacktop state)
        _ -> error "Runtime error: Insufficient operands"

    runBranchOperation code1 code2 = case stack of
        StackT:restStack -> run (code1, restStack, state)
        StackF:restStack -> run (code2, restStack, state)
        StackInt _:restStack -> error "Runtime error: Invalid operation"
        _ -> error "Runtime error: Insufficient operands"

    runLoopOperation code1 code2 = run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]], stack, state)


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","") Passou 
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")