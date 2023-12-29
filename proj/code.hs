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
      _ -> error "Runtime error: Invalid operation"

    runSubBinaryOperation = case stack of
        StackInt value1:StackInt value2:restStack -> run (code, StackInt (value1 - value2):restStack, state)
        _ -> error "Runtime error: Invalid operation"

    runMultBinaryOperation = case stack of
        StackInt value1:StackInt value2:restStack -> run (code, StackInt (value1 * value2):restStack, state)
        _ -> error "Runtime error: Invalid operation"

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
      Nothing -> error "Run-time error: Variable not found"
    
    storeValue var value state = case lookup var state of
      Just _ -> (var, value) : filter (\(var', _) -> var' /= var) state
      Nothing -> (var, value) : state

    runBranch c1 c2 = case stack of
      (StackBool True) : rest -> run (c1 ++ code, rest, state)
      (StackBool False) : rest -> run (c2 ++ code, rest, state)
      _ -> error "Run-time error: Invalid argument for branch"
        
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
compile = concatMap compileStm

compileStm :: Stm -> Code
compileStm (VarAssign var aexp) = compA aexp ++ [Store var]
compileStm (BranchS bexp stm1 stm2) = compB bexp ++ [Branch (compile stm1) (compile stm2)]
compileStm (LoopS bexp stm) = [Loop (compB bexp) (compile stm)]

parse :: String -> Program
parse str = parseaux (lexer str) []

parseaux :: [String] -> [Stm] -> [Stm]
parseaux [] stm = stm
parseaux (a:":=":rest) stm = let x = (getjustvalue (elemIndex ";" (a:":=":rest)))
                              in case parseSumOrProdOrIntOrPar (drop 2 (take (x-1) (a:":=":rest))) of
                                Just (expr,[]) -> parseaux (drop x (a:":=":rest)) (stm++[(VarAssign a (expr))])
                                Nothing -> error "Parse Error"
                                _ -> error "Parse Error"
parseaux ("(":rest) stm = parseaux (drop (getjustvalue (elemIndex ")" ("(":rest))) ("(":rest)) (stm++(parseaux (drop 1 (take ((getjustvalue (elemIndex ")" ("(":rest)))-1) ("(":rest))) []))
parseaux (";":rest) stm = parseaux rest stm
parseaux ("if":rest) stm = let thenpos = (getjustvalue (elemIndex "then" ("if":rest)))
                               elsepos = (getjustvalue (elemIndex "else" ("if":rest)))
                               arrayafter = (drop (elsepos) ("if":rest))
                            in case takefirstelement arrayafter of
                              "(" -> parseaux (drop (getjustvalue (elemIndex ")" arrayafter)) arrayafter) (stm++[BranchS (getJustvalueBexp ((parseAndandBoolEq (checkifPar (drop 1 (take (thenpos-1) ("if":rest))))))) (parseaux (drop thenpos (take (elsepos-1) ("if":rest))) []) (parseaux (take (getjustvalue (elemIndex ")" arrayafter)) arrayafter ) [] )])
                              _  -> parseaux (drop (getjustvalue (elemIndex ";" arrayafter)) arrayafter) (stm++[BranchS (getJustvalueBexp ((parseAndandBoolEq (checkifPar (drop 1 (take (thenpos-1) ("if":rest))))))) (parseaux (drop thenpos (take (elsepos-1) ("if":rest))) []) (parseaux (take (getjustvalue (elemIndex ";" arrayafter)) arrayafter ) [] )])
parseaux ("while":rest) stm = let dopos = (getjustvalue (elemIndex "do" ("while":rest)))
                                  arrayafter = (drop (dopos) ("while":rest))
                              in case takefirstelement arrayafter of
                                "(" -> parseaux (drop (getjustvalue (elemIndex ")" arrayafter)) arrayafter) (stm++[LoopS (getJustvalueBexp ((parseAndandBoolEq (checkifPar (drop 1 (take (dopos-1) ("while":rest))))))) (parseaux (take (getjustvalue (elemIndex ")" arrayafter)) arrayafter ) [] )])
                                _ -> parseaux (drop (getjustvalue (elemIndex ";" arrayafter)) arrayafter) (stm++[LoopS (getJustvalueBexp ((parseAndandBoolEq (checkifPar (drop 1 (take (dopos-1) ("while":rest))))))) (parseaux (take (getjustvalue (elemIndex ";" arrayafter)) arrayafter ) [] )])


getJustvalueBexp :: Maybe (Bexp,[String]) -> Bexp
getJustvalueBexp (Just (a,[")"])) = a
getJustvalueBexp (Just (a,[])) = a
getJustvalueBexp Nothing = error "Parse Error"

checkifPar :: [String] -> [String]
checkifPar ("(":rest) = drop 1 (take (length ("(":rest)) ("(":rest))
checkifPar rest = rest

takefirstelement :: [String] -> String
takefirstelement ("(":rest) = "("
takefirstelement (a:rest) = a

parseInt :: [String] -> Maybe (Aexp,[String])
parseInt (n:rest) =
  case (readMaybe n :: Maybe Integer) of
    Just f -> Just (Num f, rest)
    Nothing -> Just (Var n,rest)
parseInt _ = Nothing

parseProdOrInt :: [String] -> Maybe(Aexp,[String])
parseProdOrInt str =
  case parseInt str of
    Just (expr1,("*":restString1)) ->
      case parseProdOrInt restString1 of
        Just (expr2,restString2) ->
          Just (MultA expr1 expr2,restString2)
        Nothing                  -> Nothing
    result -> result

parseSumOrProdOrInt :: [String] -> Maybe(Aexp,[String])
parseSumOrProdOrInt str =
  case parseProdOrInt str of
    Just (expr1,("+":restString1)) ->
      case parseSumOrProdOrInt restString1 of
        Just (expr2,restString2) ->
          Just (AddA expr1 expr2,restString2)
        Nothing                  -> Nothing
    Just (expr1,("-":restString1)) ->
      case parseSumOrProdOrInt restString1 of
        Just (expr2,restString2) ->
          Just (SubA expr1 expr2,restString2)
        Nothing                  -> Nothing
    result -> result

parseIntOrParentExpr :: [String] -> Maybe (Aexp,[String])
parseIntOrParentExpr ("(":rest) =
  case parseSumOrProdOrIntOrPar rest of
    Just (expr,(")":restString1)) -> Just (expr,restString1)
    Just _ -> Nothing
    Nothing -> Nothing
parseIntOrParentExpr (n:rest) =
  case (readMaybe n :: Maybe Integer) of
    Just f -> Just (Num f, rest)
    Nothing -> Just (Var n,rest)
parseIntOrParentExpr _ = Nothing

parseProdOrIntOrPar :: [String] -> Maybe (Aexp,[String])
parseProdOrIntOrPar rest =
  case parseIntOrParentExpr rest of
    Just (expr1,("*":restString1)) ->
      case parseProdOrIntOrPar restString1 of
        Just (expr2,restString2) -> Just (MultA expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

parseSumOrProdOrIntOrPar :: [String] -> Maybe (Aexp,[String])
parseSumOrProdOrIntOrPar rest =
  case parseProdOrIntOrPar rest of
    Just (expr1,("+":restString1)) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2,restString2) -> Just (AddA expr1 expr2, restString2)
        Nothing -> Nothing
    Just (expr1,("-":restString1)) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2,restString2) -> Just (SubA expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

------------- PARSE Bexp ----------------

parseLessOrEqOrTrueOrFalseOrParentOrArith :: [String] -> Maybe (Bexp,[String])
parseLessOrEqOrTrueOrFalseOrParentOrArith ("(":rest) =
  case parseAndandBoolEq rest of
    Just (expr,(")":restString1)) -> Just (expr,restString1)
    Just _ -> Nothing
    Nothing -> Nothing
parseLessOrEqOrTrueOrFalseOrParentOrArith ("True":rest) = Just (TruB,rest)
parseLessOrEqOrTrueOrFalseOrParentOrArith ("False":rest) = Just (FalsB,rest)
parseLessOrEqOrTrueOrFalseOrParentOrArith rest =
  case parseSumOrProdOrIntOrPar rest of
    Just (expr1,("<=":restString1)) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2,restString2) ->
          Just (LeB expr1 expr2, restString2)
        Nothing -> Nothing
    Just (expr1,("==":restString1)) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2,restString2) ->
          Just (EquB expr1 expr2, restString2)
        Nothing -> Nothing
    result -> Nothing

parseNegAndLessAndEq :: [String] -> Maybe(Bexp, [String])
parseNegAndLessAndEq ("not":rest) =
    case parseLessOrEqOrTrueOrFalseOrParentOrArith rest of
      Just (expr1,restString1) ->
        Just (NegB expr1,restString1)
      result -> result
parseNegAndLessAndEq rest = parseLessOrEqOrTrueOrFalseOrParentOrArith rest

parseBoolEqAndNeg :: [String] -> Maybe(Bexp, [String])
parseBoolEqAndNeg rest =
  case parseNegAndLessAndEq rest of
    Just (expr1, ("=":restString1)) ->
      case parseBoolEqAndNeg restString1 of
        Just (expr2, restString2) ->
          Just (EquBoolB expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

parseAndandBoolEq :: [String] -> Maybe(Bexp,[String])
parseAndandBoolEq rest =
  case parseBoolEqAndNeg rest of
    Just (expr1, ("and":restString1)) ->
      case parseAndandBoolEq restString1 of
        Just (expr2, restString2) ->
          Just (AndB expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result


-----------------------------------------

getjustvalue :: Num a => Maybe a -> a
getjustvalue (Just a) = a+1

lexer :: String -> [String]
lexer string = lexeracc string [] []

lexeracc :: String -> [String] -> String -> [String]
lexeracc [] acc stracc | stracc == "" =  acc
                       | otherwise = (acc++[stracc])
lexeracc ('w':'h':'i':'l':'e':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["while"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["while"]) []
lexeracc (' ':rest) acc stracc
                            | stracc == "" = lexeracc rest acc []
                            | otherwise = lexeracc rest (acc++[stracc]) []
lexeracc ('i':'f':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["if"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["if"]) []
lexeracc ('t':'h':'e':'n':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["then"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["then"]) []
lexeracc ('e':'l':'s':'e':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["else"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["else"]) []
lexeracc ('*':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["*"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["*"]) []
lexeracc ('+':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["+"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["+"]) []
lexeracc ('/':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["/"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["/"]) []
lexeracc ('-':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["-"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["-"]) []
lexeracc (';':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++[";"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++[";"]) []
lexeracc ('(':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["("]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["("]) []
lexeracc (')':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++[")"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++[")"]) []
lexeracc ('<':'=':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["<="]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["<="]) []
lexeracc ('=':'=':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["=="]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["=="]) []
lexeracc ('n':'o':'t':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["not"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["not"]) []
lexeracc ('=':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["="]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["="]) []
lexeracc ('a':'n':'d':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["and"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["and"]) []
lexeracc (':':'=':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++[":="]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++[":="]) []
lexeracc ('d':'o':rest) acc stracc
                            | stracc == "" = lexeracc rest (acc++["do"]) stracc
                            | otherwise = lexeracc rest (acc++[stracc]++["do"]) []
lexeracc (a:rest) acc stracc = lexeracc rest acc (stracc++[a])

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