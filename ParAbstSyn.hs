-- Goal: Define abstract syntax for parallel program
-- Step 1: Define abstract syntax for generic c   ----Done ish ---- TODO arrays
-- ...

-- Create examples of for loops with pragma

-- TO RUN PrettyPrint:
-- putStr(prettyPrint(Prog))

-- TO RUN Pragma identifier:


-- Helper functions
listToString :: [String] -> String
listToString [] = ""
listToString (x:xs) = x ++ listToString xs 

member :: Name -> [Name] -> Bool
member y [] = False
member y (x:xs) | y==x=True | otherwise = member y xs

-- Check if the used variable is in the global variabls, shared variables
usedvariable :: Name -> Expr -> Bool 
usedvariable x (N n) = x==n
usedvariable x (O o e1 e2) = (usedvariable x e1 || usedvariable x e2)
usedvariable x (C _ e1 e2) = (usedvariable x e1 || usedvariable x e2)
usedvariable x _ = False
-- usedvariable v (N n) | ((member n x)==True)&&((member n y) == False) = n:y | otherwise = y
-- usedvariable v (O _ e1 e2) = (usedvariable x y e1 || usedvariable x y e2)
-- usedvariable v (C _ e1 e2) = (usedvariable x y e1 || usedvariable x y e2)
-- usedvariable x 

---------------------- Generic C language -------------------------------
-- Value
data Val = I Int | F Float |B Bool  -- deriving Show
ppVal :: Val -> String
ppVal (I x) = show x
ppVal (B x) = show x

-- Variable
type Name = String

-- Binary Operation
data Op = Plus | Minus | Div | Mult -- deriving Show
ppOp :: Op -> String
ppOp Plus = "+" 
ppOp Minus = "-"
ppOp Div = "/"
ppOp Mult = "*"

-- Comparision
data Comp = LessThan | LessThanEq | GreaterThan | GreaterThanEq | Eq -- deriving Show
ppComp :: Comp -> String
ppComp LessThan = " < "
ppComp LessThanEq = " <= "
ppComp GreaterThan = " > "
ppComp GreaterThanEq = " >= "

-- Expression
data Expr = V Val | A Name Expr | N Name | O Op Expr Expr | C Comp Expr Expr -- deriving Show
ppExpr :: Expr -> String
ppExpr (V x) = ppVal x
ppExpr (O op x y) = ppExpr x ++ ppOp op ++ ppExpr y
ppExpr (N x) = x
ppExpr (C comp x y) = ppExpr x ++ ppComp comp ++ ppExpr y
ppExpr (A n e) = n ++ "[" ++ ppExpr e ++ "]"
-- ppExpr (A t e) = "new " ++ ppType t ++ "[" ++ ppExpr e ++ "]"

-- Increment/decrement
data Inc = PlusEq | MinusEq | TimesEq | DivEq -- deriving Show

-- Condition for For loop
-- data Cond = Cnd Stmt Comp Expr Expr -- deriving Show
-- ppCond :: Cond -> String
-- ppCond (Cnd (s _ n st) y z l) = ppStmt (Allocate TyInt n st) ++ " " ++ n ++ ppComp y ++ ppStmt z ++ ";" ++ " " ++ ppStmt l

-- Statement
-- data Expr = Lit Int | Var Name | Plus Expr Expr | Mult Expr Expr deriving Show

-- data Stmt = Assign Name Expr | Seq [Stmt] | While Expr Stmt deriving Show
-- 																				 [Allocate    ][Cond][Inc][Body]
data Stmt = Allocate Type Name Expr | AlloArr Type Name Expr | Assign Name Expr | AssnArr Name Expr Expr | Seq [Stmt] | For Pragma Name Expr Expr Expr Stmt -- deriving Show
ppStmt :: Stmt -> String
ppStmt (Allocate t n e) = ppType t ++ n ++ " = " ++ (ppExpr e) ++ ";"
ppStmt (Assign n e) = n ++ "=" ++ (ppExpr e) ++ ";"
ppStmt (For prag n e1 e2 e3 st) = (ppPrag prag) ++ "\n" ++ "for ( int " ++ n ++ "=" ++ ppExpr e1 ++ "; " ++ ppExpr e2 ++ "; " ++ n ++ "=" ++ ppExpr e3 ++ ")\n{\n\t" ++ ppStmt st ++ "\n}\n"
ppStmt (Seq []) = ""
ppStmt (Seq (x:xs)) = ppStmt x ++ "\n" ++ ppStmt (Seq xs)
ppStmt (AlloArr t n e) = ppType t ++ "*" ++ n ++ " = new " ++ ppType t ++ "[" ++ ppExpr e ++ "];"
ppStmt (AssnArr n e1 e2) = n ++ "[" ++ ppExpr e1 ++ "] = " ++ ppExpr e2 ++ ";"

-- Program
type Prog = [Stmt]

-- State
type State = [(Name, Val)]

--Declaration
type Decl = [(Name, Type)]

--Type Language
data Type = TyInt | TyBool | TyFloat | TyArrType Type -- deriving Show
ppType :: Type -> String
ppType TyInt = "int "
ppType TyBool = "bool "
ppType TyFloat = "float "



---------------------- Pragma abstract syntax -------------------------------
-- #pragma omp parallel for default(none),shared(xcs,ycs,rs,tn),reduction(+:numHits)
-- Shared Variable
type Shared = [Name]
ppShared :: Shared -> String
ppShared [] = ""
ppShared (x:xs) | null xs = x | otherwise = x ++ "," ++ ppShared xs

data Oper = ADD | TIMES | SUB -- deriving Show
ppOper :: Oper -> String
ppOper ADD = "+:"
ppOper TIMES = "*:"
ppOper SUB = "-:"


data Default = None | S -- deriving Show
ppDefault :: Default -> String
ppDefault None = "none"
ppDefault S = "shared"

data Param = Default Default | Shared Shared | Private Shared | Reduc Op Name | Empty -- deriving Show
ppParam :: Param -> String
ppParam (Default d) = "default(" ++ ppDefault d ++ ") "
ppParam (Shared s) = "shared(" ++ ppShared s ++ ") "
ppParam (Private s) = "private(" ++ ppShared s ++ ") "
ppParam (Reduc o n) = "reduction(" ++ ppOp o ++ ":" ++ n ++ ") "
ppParam (Empty) = ""


data Pragma = Prag [Param] -- deriving Show
ppPrag :: Pragma -> String
ppPrag (Prag []) = ""
ppPrag (Prag x) = "#pragma omp parallel for " ++ listToString (map ppParam x)


---------------------- Pretty Print Functions -------------------------------
-- Re-create Project1.cpp in Haskell Syntax 
project1 :: Prog
project1 = [Allocate TyInt "x" (V (I 5)), Allocate TyInt "y" (V (I 10))]
-- project1 = [C (Assign TyInt "x"(E (V (I 5)))), C (Assign TyInt "y"(E (V (I 10))))]

-- testFor :: Prog
-- testFor = [Allocate TyInt "NUMTRIALS" (V(I 1000)), For (Prag []) TyInt "n" (V(I 0)) (C LessThan (N "n") (N "NUMTRIALS")) (O Plus (N "n") (V(I 1))) 
--  (Seq [Allocate TyFloat "xc" (A "xcs" (N "n")), Allocate TyFloat "yc" (A "ycs" (N "n")), Allocate TyFloat "r" (A "rs" (N "n")), Assign "x" (O Plus (N "x") (V(I 1)))])]


testFor2 :: Prog
testFor2 = [Allocate TyInt "NUMTRIALS" (V(I 1000)), For (Prag [Default (None), Shared ["xcs", "ycs", "rs"], Reduc Plus "x"]) "n" (V(I 0)) (C LessThan (N "n") (N "NUMTRIALS")) (O Plus (N "n") (V(I 1))) 
 (Seq [Allocate TyFloat "xc" (A "xcs" (N "n")), Allocate TyFloat "yc" (A "ycs" (N "n")), Allocate TyFloat "r" (A "rs" (N "n")), Assign "x" (O Plus (N "x") (V(I 1)))])]
-- testFor2 = [Allocate TyInt "NUMTRIALS" (V(I 1000)), For (Prag(Default (None) (Shared ["xcs", "ycs", "rs"] (Reduc ADD ["x"] Empty)))) TyInt "n" (V(I 0)) (C LessThan (N "n") (N "NUMTRIALS")) (O Plus (N "n") (V(I 1))) 
--  (Seq [Allocate TyFloat "xc" (A "xcs" (N "n")), Allocate TyFloat "yc" (A "ycs" (N "n")), Allocate TyFloat "r" (A "rs" (N "n")), Assign "x" (O Plus (N "x") (V(I 1)))])]
-- testFor2 = [Allocate TyInt "x" (V (I 5)), Allocate TyInt "y" (V (I 10)), For (Prag (Default (None) (Empty))) (Cnd (Allocate TyInt "i"(V (I 0))) (LessThan) (N "NUMTRIALS") (Assign "i" (O Plus (N "i") (V (I 1))))) (Assign "x" (O Plus (N "x") (V(I 1))))]

testArr :: Prog
testArr = [AlloArr TyFloat "xcs" (N "NUMTRIALS"), AlloArr TyFloat "ycs" (N "NUMTRIALS"), AlloArr TyFloat "rs" (N "NUMTRIALS")]



-- Print a program to look like a C++ program
prettyPrint :: Prog -> String
prettyPrint [] = ""
prettyPrint (x:xs) = (ppStmt x) ++ "\n" ++ prettyPrint xs



-- TO RUN PrettyPrint
-- putStr(prettyPrint(Prog))

-- Pramga For rules:
-- 1) Needs a for loop
-- 2) Iterator cannot change within loop
-- 3) Control expession cannot change within loop
-- 3) Loop cannot be terminated early
-- 4) Only parallelize outermost loop

-- Shared variable list rules:
-- 1) All variables must be allocated outside the loop
-- 2) Used for assignment inside the loop
-- 3) Arrays being assigned using the iterator

type SState = (Stmt, [Name])

runme :: Prog -> String
runme p = "---------------This is the original function---------------------\n"++prettyPrint(p) ++ "\n----------------------This is the new function--------------\n" ++ prettyPrint(proganalyzer p [])


-- Program analyzer
-- Input: Prog, variable list
-- Output: Prog
proganalyzer :: Prog -> [Name] -> Prog
proganalyzer [] _ = []
proganalyzer (x:xs) names = ns:(proganalyzer xs nn) where
                                                    (ns, nn) = stmtanalyzer (x, names)

-- Stmt analyzer
-- Input: Stmt, variable list
-- Output: Stmt
stmtanalyzer :: SState -> SState
stmtanalyzer ((Allocate t n e), names) = ((Allocate t n e), n:names)
stmtanalyzer ((Assign n e), names) = ((Assign n e), names)
stmtanalyzer ((For prag n e1 e2 e3 st), names) = ((For newprag n e1 e2 e3 st), names) where 
                                                                                      newprag = pragCreator (forstanalyzer (st, n, names, [], [], [], [], True))
stmtanalyzer ((Seq []), names) = ((Seq []), names)
stmtanalyzer ((Seq (x:xs)), names) = ((Seq (s:[ss])), nn) where  
                                                          (s, is) = stmtanalyzer (x,names)
                                                          (ss, nn) = (stmtanalyzer ((Seq xs), is))
stmtanalyzer ((AlloArr t n e), names) = ((AlloArr t n e), n:names)
stmtanalyzer ((AssnArr n e1 e2), names) = ((AssnArr n e1 e2), names)

-- For loop method
-- Input: Stmt, global variable list, private variable list, shared variable list, reduction list 
-- Output: SState
-- What does this function do
-- Analyzes st input of for loop to make sure:
-- 1) Iterator cannot change within loop
-- 2) Control expession cannot change within loop
-- 3) Loop cannot be terminated early
-- 4) Only parallelize outermost loop
 

-- State within For loop: stmt within loop, global names, private names, shared names, reduction names, bool for parallelization
-- type FState = ([Stmt], [Name], [Name], [Name], [Name], Bool)

-- foranalyzer :: SState -> [Name] -> [Name] -> [Name] -> [Name] -> SState
-- SState -> iterator name -> 
-- foranalyzer :: FState -> FState
-- foranalyzer ([], g, p, s, r, b) = ([], g, p, s, r, b)
-- foranalyzer ((x:xs), g, p, s, r, b) = foranalyzer(xs, g, np, ns, nr, nb) where
--                                                                          (_, _, np, ns, nr, nb) = forstanalyzer (x, g, p, s, r, b)

-- State within For loop: stmt within loop, iterator, global names, private names, shared names, reduction names, bool for parallelization
type FSState = (Stmt, Name, [Name], [Name], [Name], [Name], [Op], Bool)
-- TODO make function for add to private list
-- TODO make cases for allocating and assigning arrays
-- TODO make case for handling nested for loops
forstanalyzer :: FSState -> FSState
forstanalyzer ((Allocate t n e), i, g, p, s, r, o, b) = ((Allocate t n e), i, g, p, ns, r, o, b) where
                                                                                       ns = checkShared e g p s r
forstanalyzer ((Assign n e), i, g, p, s, r, o, b) | i==n=((Assign n e), i, g, p, s, r, o, False) | otherwise = ((Assign n e), i, g, p, ns, nr, no, b) where
                                                                                                                                           (nr, no) = checkReduction n e g p s r o
                                                                                                                                           ns = checkShared e g p s nr
forstanalyzer ((Seq []), i, g, p, s, r, o, b) = ((Seq []), i, g, p, s, r, o, b)

-- stmtanalyzer ((Seq (x:xs)), names) = ((Seq (n:[ns])), nn) where  
--                                                           (n, is) = stmtanalyzer (x,names)
--                                                           (ns, nn) = (stmtanalyzer ((Seq xs), is))
forstanalyzer ((Seq (x:xs)), i, g, p, s, r, o, b) = ((Seq (x:xs)), i, g, np, ns, nr, no, nb) where
                                                                                          (_, _, _, ip, is, ir, io, ib) = forstanalyzer(x, i, g, p, s, r, o, b)
                                                                                          (_, _, _, np, ns, nr, no, nb) = forstanalyzer((Seq xs), i, g, ip, is, ir, io, ib)
-- forstanalyzer ((Seq (x:xs)), i, g, p, s, r, o, b) = forstanalyzer((Seq xs), i, g, np, ns, nr, no, nb) where
                                                                                        --  (_, _, g, np, ns, nr, no, nb) = forstanalyzer(x, i, g, p, s, r, o, b)

forstanalyzer ((AlloArr t n e), i, g, p, s, r, o, b) = ((AlloArr t n e), i, g, p, ns, r, o, b) where
                                                                                       ns = checkShared e g p s r

-- check for reduction: variable, global, private, shared, reduc
-- TODO Find way to determine the Oper
checkReduction :: Name -> Expr -> [Name] -> [Name] -> [Name] -> [Name] -> [Op] -> ([Name], [Op])
checkReduction x (O op e1 e2) g p s r o | (usedvariable x (O op e1 e2))&&(member x g)&&((member x r)==False)==True=(x:r,op:o) | otherwise=(r,o)
checkReduction x _ g p s r o = (r,o) 


-- check for shared: variable, global, private, shared, reduc
checkShared :: Expr -> [Name] -> [Name] -> [Name] -> [Name] -> [Name]
checkShared (N n) g p s r | ((member n g)&&((member n p) == False)&&((member n s) == False)&&((member n r) == False))==True=n:s | otherwise = s
checkShared (O _ e1 e2) g p s r = ns where
                                      us = checkShared e1 g p s r
                                      ns = checkShared e2 g p us r
checkShared (C _ e1 e2) g p s r = ns where
                                      us = checkShared e1 g p s r
                                      ns = checkShared e2 g p us r
checkShared (A n e1) g p s r | ((member n g)&&((member n p) == False)&&((member n s) == False)&&((member n r) == False))==True=n:s | otherwise = s
checkShared _ g p s r = s


-- Pragma creator
-- 
-- pragCreator :: [Name] -> [Name] -> [Name] -> Pramga
pragCreator :: FSState -> Pragma
pragCreator (_, _, _, _, _, _, _, False) = Prag []
pragCreator (_, _, _, [], [], [], [], True) = Prag [Empty]
pragCreator (_, _, _, p, s, r, o, True) = Prag ([Default None, Private p, Shared s] ++ reductionCreator r o)

reductionCreator :: [Name] -> [Op] -> [Param]
reductionCreator _ [] = [Empty]
reductionCreator (x:xs) (y:ys) = (Reduc y x) : reductionCreator xs ys

-- float a = 1. + tn + tn;
-- data Expr = V Val | A Name Expr | N Name | O Op Expr Expr | C Comp Expr Expr -- deriving Show
-- Op Plus (V (I 1)) (O Plus (N tn) (N tn))


testFor3 :: Prog
testFor3 = [Allocate TyInt "NUMTRIALS" (V(I 1000)), For (Prag []) "n" (V(I 0)) (C LessThan (N "n") (N "NUMTRIALS")) (O Plus (N "n") (V(I 1))) 
 (Seq [Allocate TyFloat "xc" (A "xcs" (N "n")), Allocate TyFloat "yc" (A "ycs" (N "n")), Allocate TyFloat "r" (A "rs" (N "n")), Assign "x" (O Plus (N "x") (V(I 1)))])]

seqTest :: Prog
seqTest = [Seq[Allocate TyInt "x" (V (I 5)), Allocate TyInt "y" (V (I 10))]]

pragTest :: Prog
pragTest = [Allocate TyInt "x" (V(I 0)), Allocate TyInt "y" (V(I 0)), For (Prag []) "i" (V(I 0)) (C LessThan (N "i") (V(I 100))) (O Plus (N "i") (V(I 1))) (Seq [Assign "x" (O Plus (N "x") (V(I 1))), Assign "y" (O Plus (N "y") (V(I 1)))])]