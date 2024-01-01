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
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum)
import Data.Maybe (fromMaybe)


-- Part 1

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackValue = StackInt Integer | StackBool Bool deriving Show
type Stack = [StackValue]

createEmptyStack :: Stack
createEmptyStack = []

type State = [(String, StackValue)]

createEmptyState :: State
createEmptyState = []

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map stackValueToStr stack)
  where
    stackValueToStr (StackInt n) = show n
    stackValueToStr (StackBool b) = show b

state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ stackValueToStr val | (var, val) <- sortBy (comparing fst) state]
  where
    stackValueToStr (StackInt n) = show n
    stackValueToStr (StackBool b) = show b

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

parse :: String -> Program
parse programS = parseStatements (lexer programS) []

parseStatements :: [String] -> [Stm] -> [Stm]
parseStatements [] stm = reverse stm 
parseStatements (a:":=":rest) stm =  -- Caso de atribuição ":="
  let x = fromMaybe 0 $ fmap (+1) (elemIndex ";" (a:":=":rest))
  in case parseSumOrProdOrIntOrPar (drop 2 (take (x-1) (a:":=":rest))) of -- Se a expressão for válida, adiciona uma instrução VarAssign à lista de instruções
       Just (expr, []) -> parseStatements (drop x (a:":=":rest)) (VarAssign a expr : stm)
       _               -> error "Run-time error" -- Se a expressão não for válida, lança um erro "Parse Error"
parseStatements ("(":rest) stm =  -- Caso de expressões em parênteses
  let index = fromMaybe 0 $ fmap (+1) (elemIndex ")" ("(":rest))
  in parseStatements (drop (index + 1) ("(":rest)) (parseStatements (drop 1 (take index ("(":rest))) [] ++ stm)
parseStatements (";":rest) stm = parseStatements rest stm -- Caso de ponto e vírgula ";"
parseStatements (a:rest) stm =   -- Caso geral (identificadores, expressões)
  let x = fromMaybe 0 $ fmap (+1) (elemIndex ";" (a:rest)) -- Calcula x, que é o índice do ponto e vírgula (;) que marca o final da expressão atual.
  in case parseSumOrProdOrIntOrPar (drop 1 (take (x-1) (a:rest))) of -- Se a expressão for válida, adiciona uma instrução VarAssign à lista de instruções
       Just (expr, []) -> parseStatements (drop x rest) (VarAssign a expr : stm)
       _               -> error "Run-time error"


parseInt :: [String] -> Maybe (Aexp, [String])
parseInt [] = Nothing
parseInt (n:rest) =
  case readMaybe n of
    Just f -> Just (Num f, rest)  -- Se a conversão para número funcionar, retorna um número e a lista restante.
    _      -> Just (Var n, rest)  -- Se a conversão falhar, assume que é uma variável e retorna uma variável e a lista restante.
   


parseProdOrInt :: [String] -> Maybe(Aexp,[String])
parseProdOrInt strings =
  case parseInt strings of
    Just (expr1,("*":restString1)) ->      -- Se a análise for bem-sucedida e a próxima string for "*", continua com a análise da expressão de multiplicação.
      case parseProdOrInt restString1 of        -- Analisa a parte restante da expressão após o "*" usando parseProdOrInt.
        Just (expr2,restString2) ->            -- Se a análise for bem-sucedida, cria uma expressão de multiplicação e retorna a lista restante.
          Just (MultA expr1 expr2,restString2)
        Nothing                  -> Nothing
    result -> result      -- Se a análise inicial falhar, retorna o resultado da análise de parseInt.

parseSumOrProdOrInt :: [String] -> Maybe(Aexp,[String])  --recebe uma lista de strings e tenta analisar uma expressão que pode incluir adição, subtração ou multiplicação.
parseSumOrProdOrInt strings =
  case parseProdOrInt strings of   -- Tenta analisar a expressão como uma expressão de multiplicação usando parseProdOrInt.
    Just (expr1,("+":restString1)) -> -- Se a análise for bem-sucedida e a próxima string for "+", continua com a análise da expressão de adição.
      case parseSumOrProdOrInt restString1 of     -- Analisa a parte restante da expressão após o "+" usando parseSumOrProdOrInt.
        Just (expr2,restString2) ->
          Just (AddA expr1 expr2,restString2)
        Nothing                  -> Nothing
    Just (expr1,("-":restString1)) ->   -- Se a próxima string for "-", continua com a análise da expressão de subtração.
      case parseSumOrProdOrInt restString1 of
        Just (expr2,restString2) ->
          Just (SubA expr1 expr2,restString2)
        Nothing                  -> Nothing
    result -> result -- Se a análise inicial falhar, retorna o resultado da análise de parseProdOrInt.

parseIntOrParentExpr :: [String] -> Maybe (Aexp,[String])
parseIntOrParentExpr ("(":rest) =
  case parseSumOrProdOrIntOrPar rest of
    Just (expr,(")":restString1)) -> Just (expr,restString1)
    Just _ -> Nothing  -- Se a análise não for bem-sucedida ou o parêntese fechado não for encontrado, retorna Nothing.
    Nothing -> Nothing

-- Caso a lista comece com uma string n,
-- a função tenta converter n para um Integer usando readMaybe.
-- Se a conversão for bem-sucedida, retorna uma tupla Just com uma expressão numérica (Num) e a lista restante de strings.
-- Se a conversão não for bem-sucedida, assume que n é uma variável e retorna uma tupla Just com uma expressão de variável (Var) e a lista restante de strings.
parseIntOrParentExpr (n:rest) =
  case (readMaybe n :: Maybe Integer) of
    Just f -> Just (Num f, rest)
    Nothing -> Just (Var n,rest)
parseIntOrParentExpr _ = Nothing




parseProdOrIntOrPar :: [String] -> Maybe (Aexp, [String])
parseProdOrIntOrPar rest =
  case parseIntOrParentExpr rest of -- Tenta analisar a expressão como um número, variável ou expressão entre parênteses usando parseIntOrParentExpr.
    Just (expr1, ("*":restString1)) -> -- Se a análise for bem-sucedida e a próxima string for "*", continua com a análise da expressão de multiplicação.
      case parseProdOrIntOrPar restString1 of -- Analisa a parte restante da expressão 
        Just (expr2, restString2) -> Just (MultA expr1 expr2, restString2) -- Se a análise for bem-sucedida, cria uma expressão de multiplicação e retorna a lista restante.
        Nothing -> Nothing
    result -> result

parseSumOrProdOrIntOrPar :: [String] -> Maybe (Aexp, [String])
parseSumOrProdOrIntOrPar rest =
  case parseProdOrIntOrPar rest of -- Tenta analisar a expressão como uma expressão de multiplicação ou parênteses usando parseProdOrIntOrPar.
    Just (expr1, ("+":restString1)) -> -- Se a análise for bem-sucedida e a próxima string for "+", continua com a análise da expressão de adição.
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2, restString2) -> Just (AddA expr1 expr2, restString2) -- Se a análise for bem-sucedida, cria uma expressão de adição e retorna a lista restante
        Nothing -> Nothing
    Just (expr1, ("-":restString1)) -> -- Se a próxima string for "-", continua com a análise da expressão de subtração.
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2, restString2) -> Just (SubA expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result



----LEXER----
ifSpace :: Char -> Bool
ifSpace c = c == ' ' || c == '\t' || c == '\n'

ifDigit :: Char -> Bool
ifDigit c = c >= '0' && c <= '9'

digitToInt :: Char -> Int
digitToInt c = fromEnum c - fromEnum '0'

isAlphaNumOrUnderscore :: Char -> Bool
isAlphaNumOrUnderscore c = isAlphaNum c || c == '_'

lexer :: String -> [String]
lexer [] = []
lexer str@(chr : restStr)
  | ifSpace chr = lexer (dropWhile ifSpace str)
lexer str@(chr : _)
  | ifDigit chr =
    let (digitStr, restStr') = span ifDigit str
    in digitStr : lexer restStr'
lexer str@(chr : _)
  | isAlpha chr =
    let (varStr, restStr') = span isAlphaNumOrUnderscore str
    in varStr : lexer restStr'
lexer (':':'=' : restStr) = ":=" : lexer restStr
lexer ('<':'=' : restStr) = "<=" : lexer restStr
lexer ('>':'=' : restStr) = "<=" : lexer restStr
lexer ('=':'=' : restStr) = "==" : lexer restStr
lexer ('!':'=' : restStr) = "!=" : lexer restStr
lexer (';': restStr) = ";" : lexer restStr
lexer ('(': restStr) = "(" : lexer restStr
lexer (')': restStr) = ")" : lexer restStr
lexer ('{': restStr) = "{" : lexer restStr
lexer ('}': restStr) = "}" : lexer restStr
lexer ('+': restStr) = "+" : lexer restStr
lexer ('-': restStr) = "-" : lexer restStr
lexer ('*': restStr) = "*" : lexer restStr
lexer ('/': restStr) = "/" : lexer restStr
lexer ('&':'&': restStr) = "&&" : lexer restStr
lexer ('|':'|': restStr) = "" : lexer restStr
lexer (char : restStr) 
  | isAlpha char =
    let (varStr, restStr') = span isAlphaNumOrUnderscore restStr
    in varStr : lexer restStr'
lexer (char : restStr) 
  | otherwise = [char] : lexer restStr

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
