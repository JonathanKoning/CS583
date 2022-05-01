-- Goal: Define abstract syntax for parallel program
-- Step 1: Define abstract syntax for generic program
-- Step 2: Given a program, return boolean if it can be parallelized

-- Value
data Val = I Int | B Bool | A Int Val

-- Variable
type Name = String

-- Operation
data Op = Plus | Minus | Div | Mult | Concat

-- Expression
data Expr = V Val | O Op Expr Expr

-- Condition
data Cond = T | Not Cond

-- Command
data Cmd = Assign Name Stmt

-- Statement
data Stmt = C Cmd | E Expr | Seq Stmt Stmt | While Cond Stmt | Skip

-- Program
type Prog = [Stmt]

-- State
type State = [(Name, Val)]

--Declaration
type Decl = [(Name, Type)]

--Type Language
data Type = TyInt | TyBool | TyArrType
