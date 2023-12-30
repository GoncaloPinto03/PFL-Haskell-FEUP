-- PFL 2023/24 - Haskell practical assignment quickstart
import Data.List
import qualified Data.Map.Strict as HashMap
import Data.Map (toList)
import Language.Haskell.TH (Lit(IntegerL))
import Debug.Trace
import Data.List (sortBy)
import Data.List (elemIndex)
import Data.Ord (comparing)
import Text.Read (readMaybe)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackValue = StackInt Integer | StackBool Bool deriving Show
type Stack = [StackValue]

-- createEmptyStack :: Stack
createEmptyStack :: Stack
createEmptyStack = []

type State = [(String, StackValue)]

-- createEmptyState :: State
createEmptyState :: State
createEmptyState = []

-- stack2Str :: Stack -> String
stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map stackValueToStr stack)
  where
    stackValueToStr (StackInt n) = show n
    stackValueToStr (StackBool b) = show b

-- state2Str :: State -> String
state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ stackValueToStr val | (var, val) <- sortBy (comparing fst) state]
  where
    stackValueToStr (StackInt n) = show n
    stackValueToStr (StackBool b) = show b

-- run :: (Code, Stack, State) -> (Code, Stack, State)
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state) -- Base case: empty code, return the current state
run (inst:code, stack, state) = case inst of
  Push n -> run (code, StackInt n : stack, state)
  Add -> runAddBinaryOperation
  Mult -> runMultBinaryOperation
  Sub -> runSubBinaryOperation
  Tru -> run (code, StackBool True : stack, state)
  Fals -> run (code, StackBool False : stack, state)
  Equ -> runEquComparisonOperation
  Le -> runLeComparisonOperation
  And -> runLogicalOperation (&&)
  Neg -> runNegOperation
  Fetch var -> run (code, fetchValue var : stack, state)
  Store var -> run (code, drop 1 stack, storeValue var (head stack) state)
  Noop -> run (code, stack, state)
  Branch c1 c2 -> runBranch c1 c2
  Loop c1 c2 -> runLoop c1 c2
  where
    runAddBinaryOperation = case stack of
      StackInt value1:StackInt value2:restStack -> run (code, StackInt (value1 + value2):restStack, state)
      _ -> error "Runtime error"

    runSubBinaryOperation = case stack of
        StackInt value1:StackInt value2:restStack -> run (code, StackInt (value1 - value2):restStack, state)
        _ -> error "Runtime error"

    runMultBinaryOperation = case stack of
        StackInt value1:StackInt value2:restStack -> run (code, StackInt (value1 * value2):restStack, state)
        _ -> error "Runtime error"

    runEquComparisonOperation = case stack of
      StackInt value1 : StackInt value2 : restStack -> run (code, StackBool value : restStack, state)
        where value = value1 == value2
      StackBool value1 : StackBool value2 : restStack -> run (code, StackBool value : restStack, state)
        where value = value1 == value2
      _ -> error "Run-time error"

    runLeComparisonOperation = case stack of
      StackInt value1 : StackInt value2 : restStack -> run (code, StackBool value : restStack, state)
        where value = value1 <= value2
      _ -> error "Run-time error"

    runLogicalOperation op = case stack of
      StackBool value1 : StackBool value2 : restStack -> run (code, StackBool value : restStack, state)
        where value = value1 `op` value2
      _ -> error "Run-time error"

    runNegOperation = case stack of
      StackBool value : restStack -> run (code, StackBool (not value) : restStack, state)
      _ -> error "Run-time error"

    fetchValue var = case lookup var state of
      Just value -> value
      Nothing -> error "Run-time error"

    storeValue var value state = case lookup var state of
      Just _ -> (var, value) : filter (\(var', _) -> var' /= var) state
      Nothing -> (var, value) : state

    runBranch c1 c2 = case stack of
      (StackBool True) : rest -> run (c1 ++ code, rest, state)
      (StackBool False) : rest -> run (c2 ++ code, rest, state)
      _ -> error "Run-time error"

    runLoop code1 code2 = run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]], stack, state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"


-- Part 2
data Aexp = Num Integer | Var String | AddA Aexp Aexp | SubA Aexp Aexp | MultA Aexp Aexp  deriving Show
data Bexp = EquB Aexp Aexp | LeB Aexp Aexp | AndB Bexp Bexp | EquBoolB Bexp Bexp | NegB Bexp | TruB | FalsB  deriving Show
data Stm = BranchS Bexp [Stm] [Stm] | LoopS Bexp [Stm] | VarAssign String Aexp deriving Show
type Program = [Stm]


compA :: Aexp -> Code
compA (Num a) = [Push a]
compA (Var a) = [Fetch a]
compA (AddA a b) = compA b ++ compA a ++ [Add]
compA (SubA a b) = compA b ++ compA a ++ [Sub]
compA (MultA a b) = compA b ++ compA a ++ [Mult]

compB :: Bexp -> Code
compB (EquB a b) = compA b ++ compA a ++ [Equ]
compB (LeB a b) = compA b ++ compA a ++ [Le]
compB (AndB a b) = compB b ++ compB a ++ [And]
compB (NegB a) = compB a ++ [Neg]
compB (EquBoolB a b) = compB b ++ compB a ++ [Equ]
compB TruB = [Tru]
compB FalsB = [Fals]

compile :: Program -> Code
compile [] = []
compile (stm : stms) =
  case stm of
    VarAssign var aexp ->
      compA aexp ++ [Store var] ++ compile stms

    BranchS bexp trueBranch falseBranch ->
      compB bexp ++ [Branch (compile trueBranch) (compile falseBranch)] ++ compile stms

    LoopS bexp loopBody ->
      Loop (compB bexp) (compile loopBody) : compile stms

-- Compiler tests

testProgram1 :: Program
testProgram1 = [VarAssign "x" (Num 5), VarAssign "y" (AddA (Var "x") (Num 3))]
-- Expected output: ("","x=5,y=8")
-- Explanation: Assigns 5 to variable x and then calculates y = x + 3.

testProgram2 :: Program
testProgram2 = [VarAssign "x" (Num 5), BranchS (EquB (Var "x") (Num 5)) [VarAssign "y" (Num 10)] [VarAssign "y" (Num 20)]]
-- Expected output: ("","x=5,y=10")
-- Explanation: Checks if x is equal to 5. If true, assigns 10 to y, otherwise assigns 20.

testProgram3 :: Program
testProgram3 = [VarAssign "x" (Num 5), LoopS (LeB (Var "x") (Num 10)) [VarAssign "x" (MultA (Var "x") (Num 2))]]
-- Expected output: ("","x=20")
-- Explanation: Initializes x to 5 and then multiplies x by 2 in a loop until x is no longer less than or equal to 10.

testProgram4 :: Program
testProgram4 =
  [ VarAssign "x" (Num 5),
    VarAssign "y" (AddA (Var "x") (Num 3)),
    BranchS (AndB (LeB (Var "x") (Num 5)) (EquBoolB (NegB FalsB) TruB)) [VarAssign "z" (Num 42)] [VarAssign "z" (Num 0)]
  ]
-- Expected output: ("","x=5,y=8,z=42")
-- Explanation: Assigns values to x and y, then checks a complex condition to determine whether to set z to 42 or 0.

compileAndRun :: Program -> (String, String)
compileAndRun program = testAssembler (compile program)

-- Compiler tests
testResult1 :: (String, String)
testResult1 = compileAndRun testProgram1
-- Expected output: ("","x=5,y=8")

testResult2 :: (String, String)
testResult2 = compileAndRun testProgram2
-- Expected output: ("","x=5,y=20")

testResult3 :: (String, String)
testResult3 = compileAndRun testProgram3
-- Expected output: ("","x=40")

testResult4 :: (String, String)
testResult4 = compileAndRun testProgram4
-- Expected output: ("","x=5,y=8,z=42")

-- parse :: String -> Program
parse = undefined -- TODO

-- lexer :: String -> [String]
lexer :: String -> [String]
lexer input = removeEmptyStrings $ words $ map replaceNewlines input
  where
    removeEmptyStrings = filter (not . null)
    replaceNewlines char = if char == '\n' then ' ' else char


-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

-- Function to test Part 1 examples
testPart1 :: IO ()
testPart1 = do
  let testResults =
        [ testAssembler [Push 10, Push 4, Push 3, Sub, Mult] == ("-10", "")
        , testAssembler [Fals, Push 3, Tru, Store "var", Store "a", Store "someVar"] == ("", "a=3,someVar=False,var=True")
        , testAssembler [Fals, Store "var", Fetch "var"] == ("False", "var=False")
        , testAssembler [Push (-20), Tru, Fals] == ("False,True,-20", "")
        , testAssembler [Push (-20), Tru, Tru, Neg] == ("False,True,-20", "")
        , testAssembler [Push (-20), Tru, Tru, Neg, Equ] == ("False,-20", "")
        , testAssembler [Push (-20), Push (-21), Le] == ("True", "")
        , testAssembler [Push 5, Store "x", Push 1, Fetch "x", Sub, Store "x"] == ("", "x=4")
        , testAssembler [Push 10, Store "i", Push 1, Store "fact", Loop [Push 1, Fetch "i", Equ, Neg] [Fetch "i", Fetch "fact", Mult, Store "fact", Push 1, Fetch "i", Sub, Store "i"]] == ("", "fact=3628800,i=1")
        -- Add more test cases if needed
        ]

  if and testResults
    then putStrLn "Part 1 tests passed!"
    else putStrLn "Part 1 tests failed!"

-- Function to test Part 2 examples
testPart2 :: IO ()
testPart2 = do
  let testResults =
        [ testParser "x := 5; x := x - 1;" == ("", "x=4")
        , testParser "x := 0 - 2;" == ("", "x=-2")
        , testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("", "y=2")
        , testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("", "x=1")
        , testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("", "x=2")
        , testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("", "x=2,z=4")
        , testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("", "x=34,y=68")
        , testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("", "x=34")
        , testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("", "x=1")
        , testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("", "x=2")
        , testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("", "x=2,y=-10,z=6")
        , testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("", "fact=3628800,i=1")
        -- Add more test cases if needed
        ]

  if and testResults
    then putStrLn "Part 2 tests passed!"
    else putStrLn "Part 2 tests failed!"

-- Run the tests
main :: IO ()
main = do
  testPart1
  testPart2
