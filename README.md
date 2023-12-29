# FEUP - PFL
Functional and Logic Programming TP2 based on Haskell language.

This project is an implementation of the parsing + compiling of a simple imperative language.

## Group Identification

Group T12_G08

João Pedro Dias Fernandes - up202108044
Diogo Alexandre Oliveira da Silva - up202105327

## Part 1 - Assembler

The assembler is a low-level machine which has the Code to be executed + a Evaluation Stack to evaluate integer numbers and booleans + and a Storage to store variables and their respective values.

To begin, we defined Data and Types that we are gonna to used in the entire project:

```haskell
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]
data StackTypes =
    Inteiro Integer | Booleano Bool deriving Show
type Stack = [StackTypes]
type State = [(String, StackTypes)]
```
We have the already defined and given in the Specification file data Inst and type Code

We defined by ourselfs the data StackTypes which is composed by Inteiro Integer or Booleano Bool, because it allows us to have two different data types in Stack, which is a type of list of StackTypes.

The type State is basically a tuple with String + StackTypes, due to the same reason as the Stack and with that, we are able to associate a variable with a Integer/Boolean.

And now we have the following and relevant functions:

- ```createEmptyStack :: Stack```: Used to create an empty Stack.
  
- ```createEmptyState :: State```: Used to create an empty State.
  
- ```stack2Str :: Stack -> String```: Converts the Stack to a string, by the help of a auxiliar ```stack2StrRec``` function.
  
- ```findState :: State -> String -> Maybe StackTypes```: Given a State and a variable (String) it finds if the variable exists, if not, returns Nothing.
  
- ```subsState :: State -> String -> StackTypes -> State```: Used to substitute a variable's value in a State OR create a new variable and associate it with a value.

- ```state2Str :: State -> String```: Converts the State to a string, by sorting it by variable's name + by the help of a auxiliar ```state2StrRec``` function.

- ```run :: (Code, Stack, State) -> (Code, Stack, State)```: Runs through the Code and by function call matching (matching the Inst that is on top of code + values of State in some cases) we are able to get specific actions through specific instructions. If it doesn't match any of the function calls, it gives Run-time error, because we want to ensure that all possible and logical function calls are already included in function call matching.

## Part 2 - Compiler

Now consider a small imperative programming language with arithmetic and boolean expressions, and statements consisting of assignments of the form x := a, sequence of statements (instr1 ; instr2), if then else statements, and while loops.

In this part of the project, we are gonna to define a translation (compiler) from this language into lists of instructions in the previous machine.

To begin, we defined Data and Types that we are gonna to use in this part of project:

```haskell
data Aexp = Num Integer | Var String | AddA Aexp Aexp | SubA Aexp Aexp | MultA Aexp Aexp  deriving Show
data Bexp = EquB Aexp Aexp | LeB Aexp Aexp | AndB Bexp Bexp | EquBoolB Bexp Bexp | NegB Bexp | TruB | FalsB  deriving Show
data Stm = BranchS Bexp [Stm] [Stm] | LoopS Bexp [Stm] | VarAssign String Aexp deriving Show
type Program = [Stm]
```

As instructed, we defined Aexp for arithmetic expressions (and respective constants), Bexp for boolean expressions (and respective constants), Stm for statements and structures (Note that: in LoopS (while loop) and BranchS (If-then-else), we defined [Stm] due to the possibility of then/else/do of having more than one statement inside them) and Program, which is simply a list of Stm.

And now we have the following and relevant functions:

- ```lexer :: String -> [String]```: This function basically separates every token on the string and stores it list of strings. Note that we call ```lexeraux``` to allow to have a recursive call function and, in this lexer, the spaces between tokens aren't relevant, due to function call matching method used also here. If it doesn't match any of the function calls, it is considered a variable or a integer, and is stored into a string accumulator and, when finding a word that matches any of the function call pattern, it is stored in the accumulator string list.

- ```parseSumOrProdOrIntOrPar :: [String] -> Maybe (Aexp,[String])```: A function called in parse that helps to parse arithmetic expressions, respecting operation priority and parenthesis.

- ```parseAndandBoolEq :: [String] -> Maybe(Bexp,[String])```: A function called in parse that helps to parse boolean expressions (may contain arithmetic expressions inside it), respecting priority and parenthesis.

- ```parse :: String -> [Stm]```: The parse function that, with help of ```parseaux```, it gives the string list to ```lexer``` and, with the string list received, iterates through it, matching the each case of structure in that language such as: variable assignment of arithmetic expressions, if-then-else statments and while-do loops, respecting also parenthesis in the last two structures.

-- INSERIR COMPILER, COMPA E COMPB


- 


