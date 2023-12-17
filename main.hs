-- PFL 2023/24 - Haskell practical assignment quickstart

-- Part 1
import Debug.Trace
-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]
data StackTypes =
    Inteiro Integer | Booleano Bool deriving Show
type Stack = [StackTypes]
type State = [(String, StackTypes)]

createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str s = init (stack2StrRec s "")

stack2StrRec :: Stack -> String -> String
stack2StrRec [] acc = acc
stack2StrRec (Booleano True:rest) acc = stack2StrRec rest acc++"True,"
stack2StrRec (Booleano False:rest) acc = stack2StrRec rest acc++"False,"
stack2StrRec (Inteiro s:rest) acc = stack2StrRec rest (acc++((show s)++","))

                        
createEmptyState :: State
createEmptyState = []

findState :: State -> String -> Maybe StackTypes

findState [] _ = Nothing
findState ((str,b):code) c
                          | str == c = Just b
                          | otherwise = findState code c

getJustInt :: Maybe Integer -> StackTypes
getJustInt (Just a) = Inteiro a

subsState :: State -> String -> S -> State
subsState state str int = subsStateRec state str int []

subsStateRec :: State -> String -> Integer -> State -> State
subsStateRec [] _ _ acc = reverse acc
subsStateRec ((notstr,b):code) str c acc 
                                      | notstr == str = reverse acc ++ ((str, c) : code)
                                      | otherwise = subsStateRec code str c ((notstr,b):acc)

state2Str :: State -> String
state2Str [] = ""
state2Str state = init (state2StrRec state "")

state2StrRec :: State -> String -> String
state2StrRec [] acc = acc
state2StrRec ((str, int):code) acc = state2StrRec code (acc ++ str ++ "=" ++ show int ++ ",")

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([],stack,state) = ([],stack,state)
run (Add: code, (Inteiro v1:Inteiro v2:stackRest),state) = run (code,(Inteiro (v1+v2):stackRest),state)
run (Sub: code, (Inteiro v1:Inteiro v2:stackRest),state) = run (code,(Inteiro (v1-v2):stackRest),state)
run (Mult: code, (Inteiro v1:Inteiro v2:stackRest),state) = run (code,(Inteiro (v1*v2):stackRest),state)
run (Equ: code,(Inteiro v1:Inteiro v2:stackRest),state) 
                                | v1 == v2 =  run (code,Booleano True:stackRest,state)
                                | otherwise = run (code,Booleano False:stackRest,state)
run (Equ: code,(Booleano v1:Booleano v2:stackRest),state) 
                                | v1 == v2 =  run (code,Booleano True:stackRest,state)
                                | otherwise = run (code,Booleano False:stackRest,state)
run (Le: code,(Inteiro v1:Inteiro v2:stackRest),state)  
                                | v1 <= v2 =  run (code,Booleano True:stackRest,state)
                                | otherwise = run (code,Booleano False:stackRest,state)
run ((Push a): code, stackRest,state) = run (code,Inteiro a:stackRest,state)
run ((Fals: code), stackRest, state) = run (code,(Booleano False:stackRest),state)
run ((Tru: code), stackRest, state) = run (code,(Booleano True:stackRest),state)
run ((Fetch x): code, stackRest,state) = run (code,((getJustInt(findState state x)):stackRest),state)
run ((Store x): code, v1:stackRest,state) = run (code,stackRest,subsState state x v1)
run (Branch c1 c2: code,(Booleano boole:stackRest),state)
                                                    | boole == True = run (c1,stackRest,state)
                                                    | otherwise = run (c2,stackRest,state)
run (Noop: code,stack,state) = run (code,stack,state)
run (Branch c1 c2: code,(Inteiro boole:stackRest),state) = (Branch c1 c2: code,(Inteiro boole:stackRest),state)
run (Loop c1 c2: code,stack,state) = run ((c1 ++ [Branch (c1 ++ [Loop c1 c2]) [Noop]])++code,stack,state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

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