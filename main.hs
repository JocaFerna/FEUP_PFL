-- PFL 2023/24 - Haskell practical assignment quickstart

-- Part 1
import Debug.Trace
import Data.List (sortBy)
import Data.List (elemIndex)
import Data.Ord (comparing)
import Text.Read (readMaybe)
import qualified Data.Text as T
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
stack2StrRec (Booleano True:rest) acc = stack2StrRec rest (acc ++ "True,")
stack2StrRec (Booleano False:rest) acc = stack2StrRec rest (acc ++ "False,")
stack2StrRec (Inteiro s:rest) acc = stack2StrRec rest (acc ++ (show s) ++ ",")

                        
createEmptyState :: State
createEmptyState = []

findState :: State -> String -> Maybe StackTypes

findState [] _ = Nothing
findState ((str,b):code) c
                          | str == c = Just b
                          | otherwise = findState code c

getJustInt :: Maybe StackTypes -> StackTypes
getJustInt (Just (Inteiro a)) = Inteiro a
getJustInt (Just (Booleano a)) = Booleano a
getJustInt _ = error $ "Run-time error"

subsState :: State -> String -> StackTypes -> State
subsState state str int = subsStateRec state str int []

subsStateRec :: State -> String -> StackTypes -> State -> State
subsStateRec [] str b acc = reverse ((str,b):acc)
subsStateRec ((notstr,b):code) str c acc 
                                      | notstr == str = reverse acc ++ ((str, c) : code)
                                      | otherwise = subsStateRec code str c ((notstr,b):acc)

sortState :: State -> State
sortState = sortBy (comparing fst)



state2Str :: State -> String
state2Str [] = ""
state2Str state = init (state2StrRec (sortState state) "")

state2StrRec :: State -> String -> String
state2StrRec [] acc = acc
state2StrRec ((str, Booleano a):code) acc = state2StrRec code (acc ++ str ++ "=" ++ show a ++ ",")
state2StrRec ((str, Inteiro a):code) acc = state2StrRec code (acc ++ str ++ "=" ++ show a ++ ",")

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([],stack,state) = ([],stack,state)
run (Add: code, (Inteiro v1:Inteiro v2:stackRest),state) = run (code,(Inteiro (v1+v2):stackRest),state)
run (Sub: code, (Inteiro v1:Inteiro v2:stackRest),state) = run (code,(Inteiro (v1-v2):stackRest),state)
run (Mult: code, (Inteiro v1:Inteiro v2:stackRest),state) = run (code,(Inteiro (v1*v2):stackRest),state)
run (Equ: code,(Inteiro v1:Inteiro v2:stackRest),state) 
                                | v1 == v2 =  run (code,Booleano True:stackRest,state)
                                | otherwise = run (code,Booleano False:stackRest,state)
run (Neg: code,(Booleano True:stackRest),state) = run (code, Booleano False:stackRest,state)
run (Neg: code,(Booleano False:stackRest),state) = run (code, Booleano True:stackRest,state)
run (And: code,(Booleano v1:Booleano v2:stackRest),state)
                                                        | v1 == v2  =  run (code,Booleano True:stackRest,state)
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
                                                    | boole == True = run (c1 ++ code,stackRest,state)
                                                    | otherwise = run (c2 ++ code,stackRest,state)
run (Noop: code,stack,state) = run (code,stack,state)
run (Loop c1 c2: code,stack,state) = run ((c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]) ++ code,stack,state)
run (code,stack,state) = error $ "Run-time error" 

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

data Aexp = Num Integer | Var String | AddA Aexp Aexp | SubA Aexp Aexp | MultA Aexp Aexp  deriving Show
data Bexp = EquB Aexp Aexp | LeB Aexp Aexp | AndB Bexp Bexp | EquBoolB Bexp Bexp | NegB Bexp | TruB | FalsB  deriving Show
data Stm = BranchS Bexp [Stm] [Stm] | LoopS Bexp [Stm] | VarAssign String Aexp deriving Show
data NotSure = AExpr Aexp | BExpr Bexp deriving Show
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
compile stms = concatMap compileStm stms

compileStm :: Stm -> Code
compileStm stm = case stm of
  VarAssign var aexp -> compA aexp ++ [Store var]
  BranchS bexp stm1 stm2  -> compB bexp ++ [Branch (compile stm1) (compile stm2)]
  LoopS bexp stm     -> [Loop (compB bexp) (compile stm)]

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

listatest = ["not","True","and","2","<=","5","=","3","==","4"]

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

--processAExp :: [String] -> Stm
--processAExp (a:":=":rest) = (VarAssign a (processAExp rest))
--processAExp resto
--                | (verifyelem "(" resto) == "Found" =
processAExp = undefined

getjustvalue :: Num a => Maybe a -> a
getjustvalue (Just a) = a+1

--verifyelem :: Eq => a -> [a] -> String
--verifyelem elem list
--                  | (elemIndex elem list) == Nothing = "NotFound"
--                  | otherwise = "Found"

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
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4") True 
-- testParser "x := 0 - 2;" == ("","x=-2") True
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2") True 
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1") True
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2") True
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4") True
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68") True
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34") True
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1") True
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2") True
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6") True
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1") True
-- 1+(14*14)