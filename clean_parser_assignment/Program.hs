module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] -- to be defined


instance Parse T where
  parse = program_parse
  toString = buildString

-- Use iter to parse through the string (or, the Just value to be correct). iter will
-- identify first token, perform the correct parse, add the resulting statement to a list
-- and then continue from where that statement ended, thus parsing the entire string into
-- a list of statements
program_parse = iter Statement.parse >-> makeProgram
makeProgram = Program

exec :: T -> [Integer] -> [Integer]
exec (Program p) nums = Statement.exec p Dictionary.empty nums

-- Use list comprehension and Statement.toString to create a list of string,
-- use a simple unwords function to create a single string from the list of strings
buildString :: T -> String
buildString (Program lst) = unwords_nospace [Statement.toString s | s <- lst]

-- Duplicate
unwords_nospace :: [String] -> String
unwords_nospace [] = []
unwords_nospace (x:xs) = x ++ unwords_nospace xs