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

data Val = IntVal Integer | TT | FF
    deriving (Eq, Show)

type Stack = [Val]
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

   putStrLn ("result1: " ++ result1)
   putStrLn ("result2: " ++ result2)
   putStrLn ("result3: " ++ result3)
   putStrLn ("result4: " ++ result4)
   putStrLn ("result5: " ++ result5)
   putStrLn ("result6: " ++ result6)
   putStrLn ("result7: " ++ result7)
   putStrLn ("result8: " ++ result8)
   putStrLn ("result9: " ++ result9)


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

data Aexp = Const Integer
          | Var String
          | ADD Aexp Aexp
          | SUB Aexp Aexp
          | MUL Aexp Aexp
          deriving (Show, Eq)

data Bexp = BoolConst Bool
          | AND Bexp Bexp
          | OR Bexp Bexp
          | NOT Bexp
          | EQ Expr
          deriving (Show, Eq)

data Stm = ASS String Aexp
         | IF Bexp Stm Stm
         | WHI Bexp Stm
         | SEQ [Stm]
         deriving (Show, Eq)


type Program = [Stm]

compA :: Aexp -> Code
compA = undefined -- TODO

compB :: Bexp -> Code
compB = undefined -- TODO

compile :: Program -> Code
compile = undefined -- TODO

lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | c `elem` ";(),*-+" = [c] : lexer cs
    | c == '=' = if not (null cs) && head cs == '='
                 then "==" : lexer (tail cs)
                 else "=" : lexer cs
    | c == ':' = if not (null cs) && head cs == '='
                 then ":=" : lexer (tail cs)
                 else ":" : lexer cs
    | otherwise = let (token, rest) = span isTokenChar (c:cs)
                  in token : lexer rest
  where
    isTokenChar x = not (isSpace x || x `elem` ";(),*=-")


-- Parses an arithmetic expression and returns the expression and the remaining tokens
parseAexp :: [String] -> (Aexp, [String])
parseAexp tokens = undefined

-- Parses a boolean expression and returns the expression and the remaining tokens
parseBexp :: [String] -> (Bexp, [String])
parseBexp tokens = undefined

-- Parses a statement and returns the statement and the remaining tokens
parseStm :: [String] -> (Stm, [String])
parseStm tokens = undefined


parse :: String -> Program
parse "" = []
parse s = undefined 

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
