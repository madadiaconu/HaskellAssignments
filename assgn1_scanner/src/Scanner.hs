module Scanner where

import Data.Char
import Data.List

skipWhiteSpaces :: String -> String
skipWhiteSpaces (c:cs) | isSpace c   = skipWhiteSpaces cs
skipWhiteSpaces cs                   = cs

{-- this is a function to split a string by either spaces or '(' or ')'
    whereas brackets will become tokens  --}
tokenize :: String -> [String]
tokenize "" = []
tokenize ('(':s) = "(" : (tokenize (skipWhiteSpaces s))
tokenize (')':s) = ")" : (tokenize (skipWhiteSpaces s))
tokenize t =
    let (first, rest) = nextWordAndRest t in
    	first : (tokenize rest)

nextWordAndRest :: String -> (String, String)
nextWordAndRest text =
    nextWordAndRest' (skipWhiteSpaces text) ""
    where
        nextWordAndRest' :: String -> String -> (String, String)
        nextWordAndRest' (s:rest)     word | isSpace s   = (reverse word, rest)
        nextWordAndRest' rest@('(':_) word               = (reverse word, rest)
        nextWordAndRest' rest@(')':_) word               = (reverse word, rest)
        nextWordAndRest' []           word               = (reverse word, [])
        nextWordAndRest' (c:rest)     word               = nextWordAndRest' rest (c:word)

isValidOperand :: String -> Bool
isValidOperand (c:rest) | isNumber c =
  isNumber' rest
  where
    isNumber' "" = True
    isNumber' (c:rest) | isNumber c = isNumber' rest
    isNumber' (c:rest) = False
isValidOperand (c:rest) | isAlphaNum c =
  isAlphaNumber' rest
  where
    isAlphaNumber' "" = True
    isAlphaNumber' (c:rest) | isAlphaNum c = isAlphaNumber' rest
    isAlphaNumber' (c:rest) = False
isValidOperand op = False

isValidBinaryOperator :: String -> Bool
isValidBinaryOperator x
	| x == "+" = True
	| x == "*" = True
	| x == "/" = True
	| x == "^" = True
	| x == "^^" = True
	| x == "**" = True
	| otherwise = False

isValidUnaryOperator :: String -> Bool
isValidUnaryOperator x
	| x == "-" = True
	| otherwise = False

{-- Checks if the tokens follows the syntax of an arithmetic expression
 with the following syntax:
  Expr = BinExpr | UnExpr | Num | Var.
  BinExpr = "(" "+" Expr Expr ")".
  UnExpr = " (" "-" Expr ")".
  Num = Dig {Dig}.
  Var = Alpha {Alpha | Dig}.
  Dig   is a digit.
  Alpha is a letter.
--}
isValidExprTokens :: [String] -> (Bool, [String]) 
isValidExprTokens xs
	| (result,[]) <- checkExpressionInt xs = (result,[])
	| (result,rest) <- checkExpressionInt xs = (False, rest)

checkExpressionInt :: [String] -> (Bool, [String])
checkExpressionInt (x:xs)
	| isValidOperand x = (True, xs)
checkExpressionInt ("(":x:xs)
	| isValidBinaryOperator x =
		let (val, xs') = checkExpressionInt xs
		    (val2, xs'') = checkExpressionInt xs' in
		    if head xs'' == ")" then 
			(True, tail xs'')
		    else
			(False, xs'')
	| isValidUnaryOperator x =
		let (val, xs') = checkExpressionInt xs in
	  	    if head xs' == ")" then
			(True, tail xs')
		    else
			(False, xs')
checkExpressionInt xs = (False, xs)
