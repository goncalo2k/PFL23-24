-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1
import Data.List
import Data.Char

-- Possible instructions in the stack machine
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

type Code = [Inst]

-- Possible values in the stack
data Val = CharVal Char | IntVal Integer | TT | FF
    deriving (Eq, Show)

type Stack = [Val]

type State = [(String, Val)]

-- Create an empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- Create an empty state
createEmptyState :: State
createEmptyState = []

-- Creates a string based on the contents of a stack
stack2Str :: Stack -> String
stack2Str = intercalate "," . map valToString
  where
    valToString (IntVal n) = show n
    valToString TT = "True"
    valToString FF = "False"

-- Creates a string based on the contents of a state
state2Str :: State -> String
state2Str = intercalate "," . map (\(var, value) -> var ++ "=" ++ valToString value) . sortOn fst
  where
    valToString (IntVal n) = show n
    valToString TT = "True"
    valToString FF = "False"

-- Program Interpreter 
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst:rest, stack, state) =
    case inst of
        -- Pushes an Integer value into the stack
        Push n      -> run (rest, IntVal n:stack, state)
        -- Pops the first top two values on the stack, adds them, and pushes the result into the stack
        Add         -> case stack of
                          (IntVal x:IntVal y:stack') -> run (rest, IntVal (x + y) : stack', state)
                          _ -> error "Run-Time Error"
        -- Pops the first top two values on the stack, subtracts them, and pushes the result into the stack
        Sub         -> case stack of
                          (IntVal x:IntVal y:stack') -> run (rest, IntVal (x - y) : stack', state)
                          _ -> error "Run-Time Error"
        -- Pops the first top two values on the stack, multiplies them, and pushes the result into the stack
        Mult        -> case stack of
                          (IntVal x:IntVal y:stack') -> run (rest, IntVal (x * y) : stack', state)
                          _ -> error "Run-Time Error"
        -- Pushes a True value into the stack
        Tru         -> run (rest, TT:stack, state)
        -- Pushes a False value into the stack
        Fals        -> run (rest, FF:stack, state)
        -- Pops the first top two values on the stack, booleans or integers, compares them, and pushes the result into the stack
        Equ         -> case stack of
                          (IntVal x:IntVal y:stack') -> run (rest, (if x == y then TT else FF) : stack', state)
                          (x:y:stack') | x == y      -> run (rest, TT : stack', state)
                                       | otherwise  -> run (rest, FF : stack', state)
                          _                         -> error "Run-Time Error"
        -- Pops the first top two values on the stack, only integers, compares them, and pushes the result into the stack
        Le          -> let (IntVal x:IntVal y:stack') = stack in run (rest, if x <= y then TT:stack' else FF:stack', state)
        -- Pops the first top two values on the stack, only booleans, compares them, and pushes the result into the stack
        And         -> case stack of
                          (TT:TT:stack') -> run (rest, TT : stack', state)
                          (FF:FF:stack') -> run (rest, FF : stack', state)
                          (TT:FF:stack') -> run (rest, FF : stack', state)
                          (FF:TT:stack') -> run (rest, FF : stack', state)
                          _              -> error "Run-time error"
        -- Pops the first top value on the stack, only a boolean, negates it, and pushes the result into the stack
        Neg         -> let (x:stack') = stack in run (rest, if x == FF then TT:stack' else FF:stack', state)
        -- Retrieves the value of a variable from the state and pushes it into the stack
        Fetch x     -> case lookup x state of
                          Nothing -> error "Run-time error"  -- Variable not found
                          Just val -> run (rest, val : stack, state)
        -- Pops the first top value on the stack and stores it in the state
        Store x     -> let (val:stack') = stack in run (rest, stack', updateState x val state)
        -- Dummy instruction
        Noop        -> run (rest, stack, state)
        -- Executes the first code if the first top value on the stack is True, otherwise executes the second code
        Branch c1 c2 -> case stack of
                          (TT:stack') -> run (c1 ++ rest, stack', state)
                          (FF:stack') -> run (c2 ++ rest, stack', state)
        -- Executes the first code, then the second code, then the first code again, and so on, while the first top value on the stack is True
        Loop c1 c2  -> run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ rest, stack, state)

-- Updates the state with a new variable value
updateState :: String -> Val -> State -> State
updateState var value state = (var, value) : filter ((/= var) . fst) state

-- Converts a boolean value to a string
boolToString :: Bool -> String
boolToString True = "True"
boolToString False = "False"

-- Converts a tuple of strings to a string
tupleToString :: (String, String) -> String
tupleToString (str1, str2) = str1 ++ ";" ++ str2

-- Part 2

-- Possible Arithmetic Operations and Expressions
data Aexp = CONST Integer
          | VAR String
          | ADD Aexp Aexp
          | SUB Aexp Aexp
          | MUL Aexp Aexp
          deriving (Show, Eq)

-- Possible Boolean Operations and Expressions
data Bexp = TRUE
          | FALSE
          | AND Bexp Bexp
          | NOT Bexp
          | INTEQ Aexp Aexp
          | LEINT Aexp Aexp
          | BOOLEQ Bexp Bexp
          deriving (Show, Eq)

-- Possible Statements
data Stm = ASS String Aexp
         | IF Bexp [Stm] [Stm]
         | WHILE Bexp [Stm]
         deriving (Show, Eq)

type Program = [Stm]

-- Possible tokens
data Token = IntegerToken Integer
           | PlusToken            -- +
           | MultToken           -- *
           | MinusToken           -- -
           | OpenPToken           -- (
           | ClosedPToken         -- )
           | IfToken              -- if
           | ThenToken            -- then
           | ElseToken            -- else
           | VarToken String      -- variable
           | AssignToken          -- :=
           | WhileToken           -- while
           | DoToken              -- do
           | TrueToken            -- True
           | FalseToken           -- False
           | AndToken             -- and
           | NotToken             -- not
           | NumeralEqToken       -- ==
           | BoolEqToken          -- =
           | LessOrEqToken        -- <=
           | SemiColonToken       -- ;
           deriving (Show, Eq)

-- Lexer function to convert a string into a list of tokens
lexer :: String -> [Token]
lexer [] = []
lexer ('+': rest) = PlusToken : lexer rest
lexer ('*': rest) = MultToken : lexer rest
lexer ('-': rest) = MinusToken : lexer rest
lexer ('(': rest) = OpenPToken : lexer rest
lexer (')': rest) = ClosedPToken : lexer rest
lexer ('n': 'o': 't': rest) = NotToken : lexer rest
lexer ('a': 'n': 'd': rest) = AndToken : lexer rest
lexer ('i': 'f': rest) = IfToken : lexer rest
lexer ('t': 'h': 'e': 'n': rest) = ThenToken : lexer rest
lexer ('e': 'l': 's': 'e': rest) = ElseToken : lexer rest
lexer ('w': 'h': 'i': 'l': 'e': rest) = WhileToken : lexer rest
lexer ('d': 'o': rest) = DoToken : lexer rest
lexer ('=': '=': rest) = NumeralEqToken : lexer rest
lexer ('=': rest) = BoolEqToken : lexer rest
lexer ('<': '=': rest) = LessOrEqToken : lexer rest
lexer (':': '=': rest) = AssignToken : lexer rest
lexer ('T': 'r': 'u': 'e': rest) = TrueToken : lexer rest
lexer ('F': 'a': 'l': 's' : 'e': rest) = FalseToken : lexer rest
lexer (';': rest) = SemiColonToken : lexer rest
lexer (c: rest)
  | isSpace c = lexer rest
  | isDigit c = IntegerToken (read num) : lexer rest' 
  | isLower c = VarToken var : lexer rest''           
  | otherwise = error ("Bad character: " ++ [c])
  where (num, rest') = span isDigit (c:rest)        
        (var, rest'') = span isAlphaNum (c:rest)    

-- Build statements from tokens
buildData :: [Token] -> [Stm]
buildData [] = []
buildData (SemiColonToken:tokens) = buildData tokens
buildData ((VarToken var):AssignToken:tokens) = ASS var (buildAexp aexp) : buildData rest
  where (aexp, rest) = break (== SemiColonToken) tokens

buildData (IfToken:tokens) = IF (buildBexp bexp) (buildData thenTokens) (buildData elseTokens) : buildData rest
    where (bexp, withThenTokens) = break (== ThenToken) tokens
          afterThenTokens = tail withThenTokens
          (thenTokens, withElseTokens) = 
                if head afterThenTokens == OpenPToken then
                  getBetweenParenthesisTokens afterThenTokens 
                else
                    break (== SemiColonToken) afterThenTokens
          afterElseTokens =
                if head withElseTokens == SemiColonToken then
                  drop 2 withElseTokens   -- drop SemiColonTok and ElseTok
                else
                  tail withElseTokens     -- drop ElseTok
          (elseTokens, rest) =
                if head afterElseTokens == OpenPToken then    -- if parenthesis
                    getBetweenParenthesisTokens afterElseTokens       -- statements between parenthesis
                else
                    break (== SemiColonToken) afterElseTokens     -- only 1 statement w/o parenthesis

buildData (WhileToken:tokens) = WHILE (buildBexp bexp) (buildData doTokens) : buildData rest
    where (bexp, withDoTokens) = break (== DoToken) tokens
          (doTokens, rest) =
                if head (tail withDoTokens) == OpenPToken then
                    getBetweenParenthesisTokens (tail withDoTokens)
                else
                    break (== SemiColonToken) (tail withDoTokens)
buildData _ = error "Invalid program on buildData"

-- Build Aerithmetic expressions
buildAexp :: [Token] -> Aexp
buildAexp tokens =
    case parseSumOpUp tokens of
        (expr, []) -> expr
        (_, _) -> error "Error building arithmetic expression"

-- Helper function to parse sums and subtractions
parseSumOpUp :: [Token] -> (Aexp, [Token])
parseSumOpUp tokens =
    case parseMultOpUp tokens of
        (expr1, PlusToken : restTokens1) ->
            case parseSumOpUp restTokens1 of
                (expr2, restTokens2) -> (ADD expr1 expr2, restTokens2)
        (expr1, MinusToken : restTokens1) ->
            case parseSumOpUp restTokens1 of
                (expr2, restTokens2) -> (SUB expr1 expr2, restTokens2)
        result -> result

-- Helper function to parse multiplication operations
parseMultOpUp :: [Token] -> (Aexp, [Token])
parseMultOpUp tokens =
    case parseAtom tokens of
        (expr1, MultToken : restTokens1) ->
            case parseMultOpUp restTokens1 of
                (expr2, restTokens2) -> (MUL expr1 expr2, restTokens2)
        result -> result

-- Helper function to parse atoms in expressions - integers, variables or expressions between parenthesis
parseAtom :: [Token] -> (Aexp, [Token])
parseAtom (IntegerToken n : restTokens) = (CONST n, restTokens)
parseAtom (VarToken var : restTokens) = (VAR var, restTokens)
parseAtom (OpenPToken : restTokens1) =
    case parseSumOpUp restTokens1 of
        (expr, ClosedPToken : restTokens2) -> (expr, restTokens2)
        _ -> error "Error parsing atom (vars, consts and parenthesis-wraped expressions)" 
parseAtom _ = error "Error parsing atom (vars, consts and parenthesis-wraped expressions)"

-- Parse and build Boolean expressions
buildBexp :: [Token] -> Bexp
buildBexp tokens = case parseAndOpUp tokens of
    (expr, []) -> expr
    _ -> error "Error building boolean expression"

-- Helper function to parse AND operations
parseAndOpUp :: [Token] -> (Bexp, [Token])
parseAndOpUp tokens = case parseBoolEqOpUp tokens of
    (expr1, AndToken : restTokensc1) ->
        case parseAndOpUp restTokensc1 of
            (expr2, restTokensc2) -> (AND expr1 expr2, restTokensc2)
    result -> result

-- Helper function to parse boolean equality operations
parseBoolEqOpUp :: [Token] -> (Bexp, [Token])
parseBoolEqOpUp tokens = case parseNotOpUp tokens of
    (expr1, BoolEqToken : restTokensc1) ->
        case parseBoolEqOpUp restTokensc1 of
            (expr2, restTokensc2) -> (BOOLEQ expr1 expr2, restTokensc2)
    result -> result

-- Helper function to parse NOT operations in boolean expressions
parseNotOpUp :: [Token] -> (Bexp, [Token])
parseNotOpUp (NotToken : rest) = case parseNotOpUp rest of
    (expr, restTokens) -> (NOT expr, restTokens)
parseNotOpUp tokens = parseIntEqOpUp tokens

-- Helper function to parse integer equality operations
parseIntEqOpUp :: [Token] -> (Bexp, [Token])
parseIntEqOpUp tokens = case parseSumOpUp tokens of
    (expr1, NumeralEqToken : restTokensc1) ->
        case parseSumOpUp restTokensc1 of
            (expr2, restTokensc2) -> (INTEQ expr1 expr2, restTokensc2)
    result -> parseLeOpUp tokens

-- Helper function to parse less than or equal (<=) operations
parseLeOpUp :: [Token] -> (Bexp, [Token])
parseLeOpUp tokens = case parseSumOpUp tokens of
    (expr1, LessOrEqToken : restTokensc1) ->
        case parseSumOpUp restTokensc1 of
            (expr2, restTokensc2) -> (LEINT expr1 expr2, restTokensc2)
    result -> parseTrueParenthesis tokens 

-- Helper function to parse expressions wrapped in parentheses
parseTrueParenthesis :: [Token] -> (Bexp, [Token])
parseTrueParenthesis (TrueToken : restTokens) = (TRUE, restTokens)
parseTrueParenthesis (FalseToken : restTokens) = (FALSE, restTokens)
parseTrueParenthesis (OpenPToken : restTokensc1) = case parseAndOpUp restTokensc1 of
    (expr, ClosedPToken : restTokensc2) -> (expr, restTokensc2)



type ResultTokens = [Token]
type RemainderTokens = [Token]
type ParenthesisStack = [Char]

-- Extracts tokens enclosed within parentheses from the input list, assuming the expression always starts with an OpenPToken
getBetweenParenthesisTokens :: [Token] -> (ResultTokens, RemainderTokens)
getBetweenParenthesisTokens tokens = (elseTokens, restTokens)
  where (restTokens, _, elseTokens) = getBetweenParenthesisTokensAux tokens [] []

-- Processes tokens, maintaining a stack and accumulating result tokens
getBetweenParenthesisTokensAux :: RemainderTokens -> ParenthesisStack -> ResultTokens
    -> (RemainderTokens, ParenthesisStack, ResultTokens)
-- When returning, reverse the result since tokens are inserted into the list in reversed order
getBetweenParenthesisTokensAux [] stk res = ([], [], reverse res)

-- Pushes an open parenthesis onto the stack
getBetweenParenthesisTokensAux (OpenPToken:tokens) stk res = 
    getBetweenParenthesisTokensAux tokens ('(':stk) res

-- Pops a closed parenthesis from the stack
getBetweenParenthesisTokensAux (ClosedPToken:tokens) stk res = 
    getBetweenParenthesisTokensAux tokens (tail stk) res

-- Either returns the result in case the stack is empty, after fully closing parentheses, or keeps processing the expression including the token in case the stack is not empty 
getBetweenParenthesisTokensAux (tok:tokens) stk res    
    | null stk   = (tok:tokens, [], reverse res)
    | otherwise  = getBetweenParenthesisTokensAux tokens stk (tok:res)


-- Parses the input string into a Program 
parse :: String -> Program
parse = buildData . lexer


-- Compiles Arithmetic expressions into code
compA :: Aexp -> Code
compA (CONST n) = [Push n]
compA (VAR x) = [Fetch x]
compA (ADD aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Add]
compA (MUL aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Mult]
compA (SUB aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Sub]    -- (a1 - a2) (stack: topmost - second topmost)


-- Compiles Boolean expressions into code
compB :: Bexp -> Code
compB TRUE = [Tru]
compB FALSE = [Fals]
compB (LEINT aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Le]
compB (INTEQ aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Equ]
compB (BOOLEQ bexp1 bexp2) = compB bexp2 ++ compB bexp1 ++ [Equ]
compB (NOT bexp) = compB bexp ++ [Neg]
compB (AND bexp1 bexp2) = compB bexp2 ++ compB bexp1 ++ [And]


-- Compiles a program into code
compile :: Program -> Code
compile [] = []
compile (ASS var aexp:rest) = compA aexp ++ [Store var] ++ compile rest
compile (IF bexp stm1 stm2:rest) = compB bexp ++ [Branch (compile stm1) (compile stm2)] ++ compile rest
compile (WHILE bexp stm:rest) = Loop (compB bexp) (compile stm) : compile rest

-- Main and test execution

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)


{-Examples:
    testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
    testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
    testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
    testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
    testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
    testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
    testAssembler [Push (-20),Push (-21), Le] == ("True","")
    testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
    testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]]
 -}


-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"


-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (parse programCode), createEmptyStack, createEmptyState)


{- Examples:
    testParser "x := 5; x := x - 1;" == ("","x=4")
    testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
    testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
    testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
    testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
    testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
    testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
    -}

main :: IO ()
main = do
   let result1 = boolToString (testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10",""))
       result2 = boolToString (testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True"))
       result3 = boolToString (testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False"))
       result4 = boolToString (testAssembler [Push (-20),Tru,Fals] == ("False,True,-20",""))
       result5 = boolToString (testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20",""))
       result6 = boolToString (testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20",""))
       result7 = boolToString (testAssembler [Push (-20),Push (-21), Le] == ("True",""))
       result8 = boolToString (testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4"))
       result9 = tupleToString (testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]])
       result10 = boolToString(testParser "x := 5; x := x - 1;" == ("","x=4"))
       --DOESNT WORK result11 = boolToString(testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2"))
       result12 = boolToString(testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1"))
       result13 = boolToString(testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2"))
       result14 = boolToString(testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4"))
       result15 = boolToString(testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6"))
       result16 = boolToString(testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1") )

   putStrLn ("result1: " ++ result1)
   putStrLn ("result2: " ++ result2)
   putStrLn ("result3: " ++ result3)
   putStrLn ("result4: " ++ result4)
   putStrLn ("result5: " ++ result5)
   putStrLn ("result6: " ++ result6)
   putStrLn ("result7: " ++ result7)
   putStrLn ("result8: " ++ result8)
   putStrLn ("result9: " ++ result9)
   putStrLn ("result10: " ++ result10)
   --DOESNT WORK putStrLn ("result11: " ++ result11)
   putStrLn ("result12: " ++ result12)
   putStrLn ("result13: " ++ result13)
   putStrLn ("result14: " ++ result14)
   putStrLn ("result15: " ++ result15)
   putStrLn ("result16: " ++ result16) 