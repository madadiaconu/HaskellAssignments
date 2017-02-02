module CST where

import Parser

--- CST -----------------------------------------------------------------------

data CST =
          Atom String
        | Op String [CST]
      deriving (Eq, Read, Show)


--- parsers for CST ------------------------------------------------------------

atom :: Parser CST
atom = word `mapTo` (\w -> Atom w)

operation :: Parser CST
operation = open `andThenRetRight`
				((token "+") `orElse` (token "-") `orElse` (token "*") `orElse` (token "/")) `andThen` \symbol ->
						((multiple (expr)) `mapTo` (\list -> Op symbol list)) `andThenRetLeft` close

expr :: Parser CST
expr =  atom `orElse` operation

-------------------------------------------------------------------------------
