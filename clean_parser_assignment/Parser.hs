module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

cons(a, b) = a:b

-- Combine parsers and use >-> to apply the function snd to the result
(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd

-- Combine parsers and use >-> to apply the function fst to the result
(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst

-- Create new parser by combining char and isSpace, will take out first char
-- and check if it is a space. Use iter to repeat until it fails
spaces :: Parser String
spaces = iter (char ? isSpace)

token :: Parser a -> Parser a
token m = m #- spaces

-- Take out first char and check if it is a char with isAlpha
letter :: Parser Char
letter =  char ? isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

-- When n=0, return the empty string, otherwise use char parser recursively and concatinate the resulting
-- tuple with cons, creating a string
chars :: Int -> Parser String
chars 0 = return []
chars n =  char # chars (n-1) >-> cons

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

-- Just use accept, and if it fails use err parser to print the error
require :: String -> Parser String
require w  = accept w ! err w

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

