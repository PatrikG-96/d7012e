module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T      |
    If Expr.T Statement Statement |
    Skip                          |
    Begin [Statement]             |
    While Expr.T Statement        |
    Read String                   |
    Write Expr.T                  |
    Repeat Statement Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = (accept "skip" #- require ";") >-> buildSkip
buildSkip _ = Skip

-- If first word is "begin", parse statements and check (require) if it ends with "end", end up with a Just(x, xs) value
-- where x is a list of statements (result from iter)
begin = (accept "begin" -# iter parse #- require "end") >-> buildBegin
buildBegin e = Begin e

-- If first word is "while", parse conditional statement (discard while token), require that "do" follow but discard "do" token
-- then parse the statement within while loop. Results in a Just((x,y),xs) where x is the conditional expression and y is the statement
while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s

-- If first word is "if", parse conditional statement and discard if token, combine with result of requiring "then" and parsing the
-- statement and requiring "else", discarding "then" and "else" tokens. The resulting tuple is combined with result of parsing else statement
-- which means the result is Just(((x,y),z),xs) where x is conditional statement, y then statement, z else statement
if' = (accept "if" -# Expr.parse) # (require "then" -# parse #- require "else") # parse >-> makeIf
makeIf ((e, s1), s2) = If e s1 s2

-- If first word is "read", discard and read the next word, require ";" and discard it, results in Just(x,xs) where x is the var string
read' = accept "read" -# word #- require ";" >-> makeRead
makeRead s = Read s

-- If first word is "write", discard and parse expression, require ";" and discard. Result is Just(x,y) where x is the expression
write = accept "write" -# Expr.parse #- require ";" >-> makeWrite
makeWrite e = Write e

-- If first word is "repeat", discard and parse the statement, require "until", discard and parse the conditional expression, require
-- ";", discard resulting in Just((x,y),xs) where x is the statement and y is the conditional statement
repeat' = accept "repeat" -# parse #- require "until" # Expr.parse #- require ";" >-> buildRepeat
buildRepeat (s, e) = Repeat s e

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

-- Same as while but we just execute the statement once when the check fails
exec (Repeat s cond: stmts) dict input =
    if (Expr.value cond dict) /= 0
        then exec (s:Repeat s cond:stmts) dict input
        else exec (s:stmts) dict input

instance Parse Statement where
  parse = skip ! read' ! write ! assignment ! if' ! while ! repeat' ! begin
  toString = (\t -> buildString t 0) -- Lamba expression as I want to have 0 as a starting value for indentation


-- Use pattern matching and recursion to build statement strings. Use an integer to control indentation level
buildString :: T -> Int -> String
buildString (Assignment var expr) i = (makeIndent i) ++ var ++ ":=" ++ toString expr ++ ";\n"
buildString (Skip) i                = (makeIndent i) ++ "skip;\n"
buildString (Write expr) i          = (makeIndent i) ++ "write " ++ toString expr ++ ";\n"
buildString (If cond e1 e2) i       = (makeIndent i) ++ "if " ++ toString cond ++ " then\n" ++ (buildString e1 (i+2))++ 
                                      (makeIndent i) ++ "else\n" ++ (buildString e2 (i+2))
buildString (While cond stmt) i     = (makeIndent i) ++ "while " ++ toString cond ++ " do\n" ++ (buildString stmt (i+2))
buildString (Begin stmts) i         = (makeIndent i) ++ "begin\n" ++ unwords_nospace[buildString s (i+2) | s <- stmts] ++ 
                                      (makeIndent i) ++ "end\n"
buildString (Read var) i            = (makeIndent i) ++ "read " ++ var ++ ";\n"
buildString (Repeat stmt cond) i    = (makeIndent i) ++ "repeat\n" ++ (buildString stmt (i+2)) ++ (makeIndent i) ++
                                      "until " ++ toString cond ++ ";\n"

makeIndent :: Int -> String
makeIndent i = replicate i ' '

unwords_nospace :: [String] -> String
unwords_nospace [] = []
unwords_nospace (x:xs) = x ++ unwords_nospace xs