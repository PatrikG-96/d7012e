module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] -- to be defined


instance Parse T where
  parse = program_parse
  toString = buildString
             
program_parse = iter Statement.parse >-> makeProgram
makeProgram = Program

exec :: T -> [Integer] -> [Integer]
exec (Program p) nums = Statement.exec p Dictionary.empty nums

buildString :: T -> String
buildString (Program lst) = unlines [Statement.toString s | s <- lst]