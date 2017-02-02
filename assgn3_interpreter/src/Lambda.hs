module Lambda where

import Data.Char
import Data.List
import Debug.Trace

import Scanner
import Parser

-- Data type definition LExpr --------------------------------------------------

type Name = String

data LExpr =     V Name
              |  B Bool
              |  I Integer
              |  Eq LExpr LExpr
              |  Add LExpr LExpr
              |  Mult LExpr LExpr
              |  Sub LExpr LExpr
              |  Div LExpr LExpr
              |  And LExpr LExpr
              |  Or LExpr LExpr
              |  Not LExpr
              |  L Name LExpr
              |  FA LExpr LExpr
              |  IF LExpr LExpr LExpr
              |  LET (Name, LExpr) LExpr
              deriving (Eq)

--- instance Show --------------------------------------------------------------

instance Show LExpr where
    show (V name) = name
    show (B v) = (show v)
    show (I v) = (show v)
    show (Eq e1 e2) = "(Eq " ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (Add e1 e2) = "(Add " ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (Sub e1 e2) = "(Sub " ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (Mult e1 e2) = "(Mult " ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (Div e1 e2) = "(Div " ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (And e1 e2) = "(And " ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (Or e1 e2) = "(Or " ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (Not e) = "(Not " ++ (show e)  ++ ")"
    show (L n e) = "(L " ++ n ++ " . " ++ (show e) ++ ")"
    show (FA fn arg) = "(" ++ (show fn) ++ " " ++ (show arg) ++ ")"
    show (IF c e1 e2) = "(IF " ++ (show c) ++ " " ++ (show e1) ++ " " ++
                              (show e2) ++ ")"
    show (LET (n, d) e) = "(LET " ++ n ++ " = " ++ (show d) ++ " IN " ++
                              (show e) ++ ")"

-- Parser for LExpr ------------------------------------------------------------

lexpr :: Parser LExpr
lexpr = boolLiteral `orElse` intLiteral `orElse` variable
        `orElse` notOperation  
        `orElse` ifStructure  
        `orElse` letStructure  
        `orElse` functionApplication  
        `orElse` lambdaExpr
        `orElse` binOperation    

boolLiteral :: Parser LExpr
boolLiteral = ((token "True") `orElse` (token "False"))
                `mapTo`  \b -> if (b == "True") then (B True) else (B False)

intLiteral :: Parser LExpr
intLiteral = (word `filterBy` (all isDigit)) `mapTo` \w -> (I (read w::Integer))

variable :: Parser LExpr
variable = word `filterBy` (\(c:cs) -> isAlpha c) `mapTo` \n -> (V n)

name :: Parser Name
name = word `filterBy` (\(c:cs) -> isAlpha c)

notOperation :: Parser LExpr
notOperation =
      open
        `andThen` \_ -> token "Not"
          `andThen` \_ -> lexpr
             `andThen` \e -> close
                `andThen` \_ -> result (Not e)

ifStructure :: Parser LExpr
ifStructure =
	open
	`andThen` \_ -> token "IF"
	`andThen` \_ -> lexpr
	`andThen` \e1 -> lexpr
	`andThen` \e2 -> lexpr
	`andThen` \e3 -> close
	`andThen` \_ -> result (IF e1 e2 e3) 	

letStructure :: Parser LExpr
letStructure = 
	open
	`andThen` \_ -> token "LET"
	`andThen` \_ -> name
	`andThen` \n -> token "="
	`andThen` \_ -> lexpr
	`andThen` \e1 -> token "IN"
	`andThen` \_ -> lexpr
	`andThen` \e2 -> close
	`andThen` \_ -> result (LET (n, e1) e2)

functionApplication :: Parser LExpr
functionApplication =
	open
	`andThen` \_ -> lexpr
	`andThen` \e1 -> lexpr
	`andThen` \e2 -> close
	`andThen` \_ -> result (FA e1 e2)

lambdaExpr :: Parser LExpr
lambdaExpr =
	open
	`andThen` \_ -> token "L"
	`andThen` \_ -> name
	`andThen` \n -> token "."
	`andThen` \_ -> lexpr
	`andThen` \e -> close
	`andThen` \_ -> result (L n e)

binOperation :: Parser LExpr
binOperation = 
	open
	`andThen` \_ -> (token "Add") `orElse` (token "Mult")
			 `orElse` (token "Div")
 			 `orElse` (token "Sub")
			 `orElse` (token "And")
			 `orElse` (token "Or")
			 `orElse` (token "Eq")	
	`andThen` \op -> lexpr
	`andThen` \e1 -> lexpr
	`andThen` \e2 -> close
	`andThen` \_ -> result (binOp op e1 e2)

binOp :: String -> LExpr -> LExpr -> LExpr
binOp op e1 e2 = case op of
	"Add" -> (Add e1 e2)
	"Mult" -> (Mult e1 e2)
	"Div" -> (Div e1 e2)
	"Sub" -> (Sub e1 e2)
	"And" -> (And e1 e2)
	"Or" -> (Or e1 e2)
	"Eq" -> (Eq e1 e2) 

--- interpret ------------------------------------------------------------------

interpretPgrm :: String -> Maybe LExpr
interpretPgrm pgrmText =
  case lexpr (tokenize pgrmText) of
    (Just ast, _) -> Just (reduce ast)
    (Nothing, _) -> Nothing

interpretParseResult :: (Maybe LExpr, [String]) -> Maybe LExpr
interpretParseResult (Just ast, _) = Just (reduce ast)
interpretParseResult (Nothing, _) = Nothing

--- Reduction   ----------------------------------------------------------------
--- reduces the expression and returns a reduced expression --------------------

reduce ::  LExpr -> LExpr
reduce expr = reduce' [] expr

--- reduce' defs expr  ---------------------------------------------------------
--- reduces the expression under variable bindings defs ------------------------

reduce' ::  [(String, LExpr)] -> LExpr -> LExpr

-- boolean literals reduce to themselves
reduce' _ (e @ (B v))      =
    trace (" --> " ++ (show e)) e

-- integer literals reduce to themselves
reduce' _ (e @ (I v))      =
    trace (" --> " ++ (show e)) e

-- variables reduce to their bindings if exists, otherwise to themselves
reduce' defs e@(V n) =
    case find (\ (n' , _) -> n == n') defs of
        Just (name, def) ->
           trace (" --> " ++ (show e)) (reduce' defs def)
        Nothing          -> V n

-- Adds are reduced by first reducing the operand expressions.
-- If both reduce to integers then add the values and return new integer literal
-- Otherwise signal an error
reduce' defs e@(Add a b) =
    let a' = reduce' defs a
        b' = reduce' defs b
    in
        trace (" --> " ++ (show e))
          (case (a', b') of
            ((I x), (I y)) ->  I (x + y)
            _ ->  error "Add")

reduce' defs e@(Mult a b) =
    let a' = reduce' defs a
        b' = reduce' defs b
    in
        trace (" --> " ++ (show e))
          (case (a', b') of
            ((I x), (I y)) ->  I (x * y)
            _ ->  error "Mult")

reduce' defs e@(Sub a b) =
    let a' = reduce' defs a
        b' = reduce' defs b
    in
        trace (" --> " ++ (show e))
          (case (a', b') of
            ((I x), (I y)) ->  I (x - y)
            _ ->  error "Sub")

reduce' defs e@(Div a b) =
    let a' = reduce' defs a
        b' = reduce' defs b
    in
        trace (" --> " ++ (show e))
          (case (a', b') of
            ((I x), (I y)) ->  I (quot x y)
            _ ->  error "Div")

reduce' defs e@(And a b) =
    let a' = reduce' defs a
        b' = reduce' defs b
    in
        trace (" --> " ++ (show e))
          (case (a', b') of
            ((B x), (B y)) ->  B (x && y)
            _ ->  error "And")

reduce' defs e@(Or a b) =
    let a' = reduce' defs a
        b' = reduce' defs b
    in
        trace (" --> " ++ (show e))
          (case (a', b') of
            ((B x), (B y)) ->  B (x || y)
            _ ->  error "Or")


reduce' defs e@(Not a) =
    let a' = reduce' defs a
    in
        trace (" --> " ++ (show e))
          (case a' of
            (B x) ->  B (not x)
            _ ->  error "Not")

reduce' defs e@(Eq a b) =
    let a' = reduce' defs a
        b' = reduce' defs b
    in
        trace (" --> " ++ (show e))
          B (a' == b')

reduce' defs e@(IF e1 e2 e3) =
    let e1' = reduce' defs e1
    in
        trace (" --> " ++ (show e))
          (case e1' of
            (B True) ->  reduce' defs e2
            (B False) ->  reduce' defs e3
            _ ->  error "IF")

reduce' defs e@(LET (n,e1) e2) =
    let b = betaReduction (L n e2) e1 
    in
        trace (" --> " ++ (show e))
		reduce' defs b



reduce' defs e@(FA e1 e2) =
    let e1' = reduce' defs e1
    in
        trace (" --> " ++ (show e))
          (case e1' of
            (L n expr) ->  
		let r = betaReduction e1' e2
		in
			reduce' defs r
            _ ->  error "FA")

betaReduction :: LExpr -> LExpr -> LExpr
betaReduction (L name expr) a = betaReduction' name expr a 
	where
	betaReduction' n (V x) a | (n == x) = a
	betaReduction' n (V x) a = (V x)
	betaReduction' n (B x) a = (B x)
	betaReduction' n (I x) a = (I x)
	betaReduction' n (FA e1 e2) a = FA (betaReduction' n e1 a) (betaReduction' n e2 a)
	betaReduction' n (Eq e1 e2) a = Eq (betaReduction' n e1 a) (betaReduction' n e2 a)
	betaReduction' n (Add e1 e2) a = Add (betaReduction' n e1 a) (betaReduction' n e2 a)
	betaReduction' n (Mult e1 e2) a = Mult (betaReduction' n e1 a) (betaReduction' n e2 a)
	betaReduction' n (Div e1 e2) a = Div (betaReduction' n e1 a) (betaReduction' n e2 a)
	betaReduction' n (Sub e1 e2) a = Sub (betaReduction' n e1 a) (betaReduction' n e2 a)
	betaReduction' n (And e1 e2) a = And (betaReduction' n e1 a) (betaReduction' n e2 a)
	betaReduction' n (Or e1 e2) a = Or (betaReduction' n e1 a) (betaReduction' n e2 a)
	betaReduction' n (Not e1) a = Not (betaReduction' n e1 a)
	betaReduction' n (IF e1 e2 e3) a = IF (betaReduction' n e1 a) (betaReduction' n e2 a) (betaReduction' n e3 a)
	betaReduction' n (L name expr) a = betaReduction' name expr a 


{-betaReduction ex@(L name (V x)) a | (name == x) = a
betaReduction ex@(L name (V x)) a = (V x)
betaReduction ex@(L name (L name' expr)) a | (name == name') = L name expr 
betaReduction ex@(L name (L name' expr)) a = L name' (betaReduction (L name expr) a)
betaReduction ex@(L name expr) a = (L name (betaReduction expr a))
betaReduction (FA e1 e2) a = FA (betaReduction e1 a) (betaReduction e2 a)
betaReduction (Add e1 e2) a = Add (betaReduction e1 a) (betaReduction e2 a)
betaReduction (Mult e1 e2) a = Mult (betaReduction e1 a) (betaReduction e2 a)
betaReduction (Div e1 e2) a = Div (betaReduction e1 a) (betaReduction e2 a)
betaReduction (Sub e1 e2) a = Sub (betaReduction e1 a) (betaReduction e2 a)
betaReduction (Not e1) a = Not (betaReduction e1 a)
betaReduction (And e1 e2) a = And (betaReduction e1 a) (betaReduction e2 a)
betaReduction (Or e1 e2) a = Or (betaReduction e1 a) (betaReduction e2 a)
betaReduction (B el) a = (B el)
-}



-- TODO: LET, FA

--------------------------------------------------------------------------------
