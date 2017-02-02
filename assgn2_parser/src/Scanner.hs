module Scanner where

import Data.Char

skipWhiteSpaces :: String -> String
skipWhiteSpaces (c:cs) | isSpace c   = skipWhiteSpaces cs
skipWhiteSpaces cs                   = cs


{-- this is a function to split a string by either spaces or '(' or ')' 
    whereas parantheses will become tokens  --}
tokenize :: String -> [String]
tokenize "" = []
tokenize text = 
   case nextToken text of 
      ("", _)      ->  []
      (tkn, rest)  ->  tkn : (tokenize rest)
   where 
      nextToken text = 
           case (skipWhiteSpaces text) of 
               ('(':rest)    -> ("(", rest)
               (')':rest)    -> (")", rest)
               text1         -> nextWordAndRest text1 


nextWordAndRest :: String -> (String, String) 
nextWordAndRest text = 
    nextWordAndRest' (skipWhiteSpaces text) ""
    where 
        nextWordAndRest' :: String -> String -> (String, String)
        nextWordAndRest' (s:rest)   tkn | isSpace s   = (reverse tkn, rest)
        nextWordAndRest' rest@('(':_) tkn             = (reverse tkn, rest)
        nextWordAndRest' rest@(')':_) tkn             = (reverse tkn, rest)
        nextWordAndRest' []         tkn               = (reverse tkn, [])
        nextWordAndRest' (c:rest)   tkn               = nextWordAndRest' rest (c:tkn)

        