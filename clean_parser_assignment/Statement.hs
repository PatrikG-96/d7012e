module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip |
    Begin [Statement] |
    While Expr.T Statement |
    Read String |
    Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = (accept "skip" #- require ";") >-> buildSkip
buildSkip _ = Skip

begin = (accept "begin" -# iter parse #- require "end") >-> buildBegin
buildBegin e = Begin e

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s

if' = (accept "if" -# Expr.parse) # (require "then" -# parse #- require "else") # parse >-> makeIf
makeIf ((e, s1), s2) = If e s1 s2

read' = accept "read" -# word #- require ";" >-> makeRead
makeRead s = Read s

write = accept "write" -# Expr.parse #- require ";" >-> makeWrite
makeWrite e = Write e


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

-- Execute the next statements with a new entry in the dictionary for the assignment
exec (Assignment var expr : stmts) dict input = 
    exec stmts (Dictionary.insert (var, Expr.value expr dict) dict) input

-- Parses value of expression and adds it to the head of the integer list
exec (Write expr : stmts) dict input = 
    Expr.value expr dict : exec stmts dict input

-- Get the head of the integer list and "bind" it to the var name by adding a dictionary entry
exec (Read var : stmts) dict input = 
    exec stmts (Dictionary.insert (var, head input) dict) (tail input)

-- Nothing
exec (Skip : stmts) dict input = 
    exec stmts dict input

-- Add the list of statements to the head of the statement list
exec (Begin s : stmts) dict input = 
    exec (s++stmts) dict input

-- Check if condition is evaluated to zero, if it is isn't perform all statements in while loop
exec (While cond s : stmts) dict input =
    if(Expr.value cond dict) /= 0
        then exec (s:While cond s:stmts) dict input -- In order to keep the loop going, we need to do the condition check after the while statements
        else exec stmts dict input -- Just execute the rest


instance Parse Statement where
  parse = skip ! read' ! write ! assignment ! if' ! while ! begin
  toString = (\t -> buildString t 0)

buildString :: T -> Int -> String
buildString (Assignment var expr) indent = (makeIndent indent) ++ var ++ ":=" ++ toString expr ++ ";"
buildString (Skip) indent = (makeIndent indent) ++ "skip;\n"
buildString (Write expr) indent = (makeIndent indent) ++ "write " ++ toString expr ++ ";"
buildString (If cond e1 e2) indent = (makeIndent indent) ++ "if " ++ toString cond ++ " then\n" ++ (buildString e1 (indent+2)) ++ (makeIndent indent) ++ "else\n" ++ 
    (buildString e2 (indent+2))
buildString (While cond stmt) indent = (makeIndent indent) ++ "while " ++ toString cond ++ " do\n" ++ (buildString stmt (indent+2))
buildString (Begin stmts) indent = (makeIndent indent) ++ "begin\n" ++ unlines[buildString s (indent+2) | s <- stmts] ++ (makeIndent indent) ++ "end"
buildString (Read var) indent = (makeIndent indent) ++ "read " ++ var ++ ";"

makeIndent :: Int -> String
makeIndent i = replicate i ' '