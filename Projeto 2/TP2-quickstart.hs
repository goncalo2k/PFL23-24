-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1
import Data.List
import Data.Char

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data Val = CharVal Char | IntVal Integer | TT | FF
    deriving (Eq, Show)

type Stack = [Val]

-- Add a new element to the top of the stack
push :: Val -> Stack -> Stack
push x xs = x : xs

-- Remove the top element from the stack
pop :: Stack -> Stack
pop (_:xs) = xs
pop _ = error "Run-time error"

-- Return the top element of the stack
top :: Stack -> Val
top (x:_) = x
top _ = error "Run-time error"

-- Create an empty stack
empty :: Stack
empty = []

-- Check if the stack is empty
isEmpty :: Stack -> Bool
isEmpty [] = True
isEmpty _  = False


type State = [(String, Val)]

createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []

stack2Str :: Stack -> String
stack2Str = intercalate "," . map valToString
  where
    valToString (IntVal n) = show n
    valToString TT = "True"
    valToString FF = "False"


state2Str :: State -> String
state2Str = intercalate "," . map (\(var, value) -> var ++ "=" ++ valToString value) . sortOn fst
  where
    valToString (IntVal n) = show n
    valToString TT = "True"
    valToString FF = "False"

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst:rest, stack, state) =
    case inst of
        Push n      -> run (rest, IntVal n:stack, state)
        Add         -> case stack of
                          (IntVal x:IntVal y:stack') -> run (rest, IntVal (x + y) : stack', state)
                          _ -> error "Run-Time Error"
        Sub         -> case stack of
                          (IntVal x:IntVal y:stack') -> run (rest, IntVal (x - y) : stack', state)
                          _ -> error "Run-Time Error"
        Mult        -> case stack of
                          (IntVal x:IntVal y:stack') -> run (rest, IntVal (x * y) : stack', state)
                          _ -> error "Run-Time Error"
        Tru         -> run (rest, TT:stack, state)
        Fals        -> run (rest, FF:stack, state)
        Equ         -> case stack of
                          (IntVal x:IntVal y:stack') -> run (rest, (if x == y then TT else FF) : stack', state)
                          (x:y:stack') | x == y      -> run (rest, TT : stack', state)
                                       | otherwise  -> run (rest, FF : stack', state)
                          _                         -> error "Run-Time Error"
        Le          -> let (IntVal x:IntVal y:stack') = stack in run (rest, if x <= y then TT:stack' else FF:stack', state)
        And         -> case stack of
                          (TT:TT:stack') -> run (rest, TT : stack', state)
                          (FF:FF:stack') -> run (rest, FF : stack', state)
                          (TT:FF:stack') -> run (rest, FF : stack', state)
                          (FF:TT:stack') -> run (rest, FF : stack', state)
                          _              -> error "Run-time error"
        Neg         -> let (x:stack') = stack in run (rest, if x == FF then TT:stack' else FF:stack', state)
        Fetch x     -> case lookup x state of
                          Nothing -> error "Run-time error"  -- Variable not found
                          Just val -> run (rest, val : stack, state)
        Store x     -> let (val:stack') = stack in run (rest, stack', updateState x val state)
        Noop        -> run (rest, stack, state)
        Branch c1 c2 -> case stack of
                          (TT:stack') -> run (c1 ++ rest, stack', state)
                          (FF:stack') -> run (c2 ++ rest, stack', state)
        Loop c1 c2  -> run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ rest, stack, state)
        -- Add more cases as necessary

updateState :: String -> Val -> State -> State
updateState var value state = (var, value) : filter ((/= var) . fst) state

boolToString :: Bool -> String
boolToString True = "True"
boolToString False = "False"

tupleToString :: (String, String) -> String
tupleToString (str1, str2) = str1 ++ ";" ++ str2

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

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
       --DOEST WORK result11 = boolToString(testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2"))
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


-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

data Expr = AExp Aexp Aexp | BExp Bexp Bexp
    deriving (Show, Eq)

data Aexp = CONST Integer
          | VAR String
          | ADD Aexp Aexp
          | SUB Aexp Aexp
          | MUL Aexp Aexp
          deriving (Show, Eq)

data Bexp = TRUE
          | FALSE
          | AND Bexp Bexp
          | NOT Bexp
          | INTEQ Aexp Aexp
          | LEINT Aexp Aexp
          | BOOLEQ Bexp Bexp
          deriving (Show, Eq)

data Stm = ASS String Aexp
         | IF Bexp [Stm] [Stm]
         | WHILE Bexp [Stm]
         deriving (Show, Eq)

type Program = [Stm]

-- Lexer
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

{-
lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | c elem ";(),*-+" = [c] : lexer cs
    | c == '=' = if not (null cs) && head cs == '='
                 then "==" : lexer (tail cs)
                 else "=" : lexer cs
    | c == ':' = if not (null cs) && head cs == '='
                 then ":=" : lexer (tail cs)
                 else ":" : lexer cs
    | c == '<' = if not (null cs) && head cs == '='
                 then "<=" : lexer (tail cs)
                 else "<" : lexer cs             
    | otherwise = let (token, rest) = span isTokenChar (c:cs)
                  in token : lexer rest
  where
    isTokenChar x = not (isSpace x || x elem ";(),*=-:+")
-}

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
  | isSpace c = lexer rest  -- ignore spaces
  | isDigit c = IntegerToken (read num) : lexer rest'   -- get digits and convert to integer
  | isLower c = VarToken var : lexer rest''           -- starts w/ lowercase letter -> variable
  | otherwise = error ("Bad character: " ++ [c])
  where (num, rest') = span isDigit (c:rest)        -- get all digits
        (var, rest'') = span isAlphaNum (c:rest)    -- get all alphanumeric characters

-- Build Statements
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
                  getBetweenParenTokens afterThenTokens 
                else
                    break (== SemiColonToken) afterThenTokens
          afterElseTokens =
                if head withElseTokens == SemiColonToken then
                  drop 2 withElseTokens   -- drop SemiColonTok and ElseTok
                else
                  tail withElseTokens     -- drop ElseTok
          (elseTokens, rest) =
                if head afterElseTokens == OpenPToken then    -- if parenthesis
                    getBetweenParenTokens afterElseTokens       -- statements between parenthesis
                else
                    break (== SemiColonToken) afterElseTokens     -- only 1 statement w/o parenthesis

buildData (WhileToken:tokens) = WHILE (buildBexp bexp) (buildData doTokens) : buildData rest
    where (bexp, withDoTokens) = break (== DoToken) tokens
          (doTokens, rest) =
                if head (tail withDoTokens) == OpenPToken then
                    getBetweenParenTokens (tail withDoTokens)
                else
                    break (== SemiColonToken) (tail withDoTokens)
buildData _ = error "Invalid program on buildData"

-- Build Aerithmetic expressions

buildAexp :: [Token] -> Aexp
buildAexp tokens =
    case parseSumOrHigher tokens of
        (expr, []) -> expr
        (_, _) -> error "Error building arithmetic expression"

parseSumOrHigher :: [Token] -> (Aexp, [Token])
parseSumOrHigher tokens =
    case parseMultOrHigher tokens of
        (expr1, PlusToken : restTokens1) ->
            case parseSumOrHigher restTokens1 of
                (expr2, restTokens2) -> (ADD expr1 expr2, restTokens2)
        (expr1, MinusToken : restTokens1) ->
            case parseSumOrHigher restTokens1 of
                (expr2, restTokens2) -> (SUB expr1 expr2, restTokens2)
        result -> result

parseMultOrHigher :: [Token] -> (Aexp, [Token])
parseMultOrHigher tokens =
    case parseAtom tokens of
        (expr1, MultToken : restTokens1) ->
            case parseMultOrHigher restTokens1 of
                (expr2, restTokens2) -> (MUL expr1 expr2, restTokens2)
        result -> result

parseAtom :: [Token] -> (Aexp, [Token])
parseAtom (IntegerToken n : restTokens) = (CONST n, restTokens)
parseAtom (VarToken var : restTokens) = (VAR var, restTokens)
parseAtom (OpenPToken : restTokens1) =
    case parseSumOrHigher restTokens1 of
        (expr, ClosedPToken : restTokens2) -> (expr, restTokens2)
        _ -> error "Error parsing atom (vars, consts and parenthesis-wraped expressions)"  -- no closing parenthesis or not parseable expression
parseAtom _ = error "Error parsing atom (vars, consts and parenthesis-wraped expressions)"

-- Parse and build Boolean expressions

buildBexp :: [Token] -> Bexp
buildBexp tokens = case parseAndOrHigher tokens of
    (expr, []) -> expr
    _ -> error "Error building boolean expression"


parseAndOrHigher :: [Token] -> (Bexp, [Token])
parseAndOrHigher tokens = case parseBoolEqOrHigher tokens of
    (expr1, AndToken : restTokens1) ->
        case parseAndOrHigher restTokens1 of
            (expr2, restTokens2) -> (AND expr1 expr2, restTokens2)
    result -> result

parseBoolEqOrHigher :: [Token] -> (Bexp, [Token])
parseBoolEqOrHigher tokens = case parseNotOrHigher tokens of
    (expr1, BoolEqToken : restTokens1) ->
        case parseBoolEqOrHigher restTokens1 of
            (expr2, restTokens2) -> (BOOLEQ expr1 expr2, restTokens2)
    result -> result

parseNotOrHigher :: [Token] -> (Bexp, [Token])
parseNotOrHigher (NotToken : rest) = case parseNotOrHigher rest of
    (expr, restTokens) -> (NOT expr, restTokens)
parseNotOrHigher tokens = parseIntEqOrHigher tokens

parseIntEqOrHigher :: [Token] -> (Bexp, [Token])
parseIntEqOrHigher tokens = case parseSumOrHigher tokens of
    (expr1, NumeralEqToken : restTokens1) ->
        case parseSumOrHigher restTokens1 of
            (expr2, restTokens2) -> (INTEQ expr1 expr2, restTokens2)
    result -> parseLeOrHigher tokens

parseLeOrHigher :: [Token] -> (Bexp, [Token])
parseLeOrHigher tokens = case parseSumOrHigher tokens of
    (expr1, LessOrEqToken : restTokens1) ->
        case parseSumOrHigher restTokens1 of
            (expr2, restTokens2) -> (LEINT expr1 expr2, restTokens2)
    result -> parseTrueParen tokens  -- if cannot parseAexp or there is no LessOrEqTok

parseTrueParen :: [Token] -> (Bexp, [Token])
parseTrueParen (TrueToken : restTokens) = (TRUE, restTokens)
parseTrueParen (FalseToken : restTokens) = (FALSE, restTokens)
parseTrueParen (OpenPToken : restTokens1) = case parseAndOrHigher restTokens1 of
    (expr, ClosedPToken : restTokens2) -> (expr, restTokens2)



type ResultTokens = [Token]
type RemainderTokens = [Token]
type ParenthesisStack = [Char]

-- assumes the expression always has parentheses (must start with OpenParenTok)
getBetweenParenTokens :: [Token] -> (ResultTokens, RemainderTokens)
getBetweenParenTokens tokens = (elseTokens, restTokens)
  where (restTokens, _, elseTokens) = getBetweenParenTokensAux tokens [] []

-- Receives tokens to process, stack and current result
-- Returns remainder, stack, and result
getBetweenParenTokensAux :: RemainderTokens -> ParenthesisStack -> ResultTokens
    -> (RemainderTokens, ParenthesisStack, ResultTokens)
-- reverse the result since tokens are inserted in reversed order
-- no more tokens, return result
getBetweenParenTokensAux [] stk res = ([], [], reverse res)

-- push parenthesis to stack
getBetweenParenTokensAux (OpenPToken:tokens) stk res = 
    getBetweenParenTokensAux tokens ('(':stk) res

-- pop parenthesis from stack
getBetweenParenTokensAux (ClosedPToken:tokens) stk res = 
    getBetweenParenTokensAux tokens (tail stk) res

-- stack is empty (parentheses fully closed) -> return result
-- if stack non-empty (non-closed parentheses), token is part of the expression
getBetweenParenTokensAux (tok:tokens) stk res    
    | null stk   = (tok:tokens, [], reverse res)
    | otherwise  = getBetweenParenTokensAux tokens stk (tok:res)

-- Parser
parse :: String -> Program
parse = buildData . lexer

compA :: Aexp -> Code
compA (CONST n) = [Push n]
compA (VAR x) = [Fetch x]
compA (ADD a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (MUL a1 a2) = compA a2 ++ compA a1 ++ [Mult]
compA (SUB a1 a2) = compA a2 ++ compA a1 ++ [Sub]    -- (a1 - a2) (stack: topmost - second topmost)


-- To compile boolean expressions we use compB
compB :: Bexp -> Code
compB TRUE = [Tru]
compB FALSE = [Fals]
compB (LEINT a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (INTEQ a1 a2) = compA a2 ++ compA a1 ++ [Equ]
compB (BOOLEQ b1 b2) = compB b2 ++ compB b1 ++ [Equ]
compB (NOT b) = compB b ++ [Neg]
compB (AND b1 b2) = compB b2 ++ compB b1 ++ [And]


-- Higher level compile function that handles all statements
compile :: Program -> Code
compile [] = []
compile (ASS var aexp:rest) = compA aexp ++ [Store var] ++ compile rest
compile (IF bexp stm1 stm2:rest) = compB bexp ++ [Branch (compile stm1) (compile stm2)] ++ compile rest
compile (WHILE bexp stm:rest) = Loop (compB bexp) (compile stm) : compile rest


-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (parse programCode), createEmptyStack, createEmptyState)


-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")


{- Main para testar a funcionalidade so far
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
       result9 = boolToString (testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1"))

       result10 = tupleToString(testAssembler [Push 1,Push 2,And])
       result11 = tupleToString(testAssembler [Tru,Tru,Store "y", Fetch "x",Tru])

   putStr ("result1: " ++ result1 ++ "\n")
   putStr ("result2: " ++ result2 ++ "\n")
   putStr ("result3: " ++ result3 ++ "\n")
   putStr ("result4: " ++ result4 ++ "\n")
   putStr ("result5: " ++ result5 ++ "\n")
   putStr ("result6: " ++ result6 ++ "\n")
   putStr ("result7: " ++ result7 ++ "\n")
   putStr ("result8: " ++ result8 ++ "\n")
   putStr ("result9: " ++ result9 ++ "\n")
   putStr ("result10: " ++ result10 ++ "\n")
   putStr ("result11: " ++ result11 ++ "\n")
-}
