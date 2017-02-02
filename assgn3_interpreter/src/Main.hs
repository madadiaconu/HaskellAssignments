module Main where

import Scanner
import Lambda
import System.IO

-- main ------------------------------------------------------------------------

main::IO ()
main =  do
        putStr "Program file: "
        fName <- getLine  -- e.g. "..\tests\P1_xOr.el"
        fHandle <- openFile fName ReadMode
        prgrm <- hGetContents fHandle
        let parseResult = lexpr (tokenize prgrm)
        print ("Parse result = " ++ show  parseResult)
        let resultExpr = interpretParseResult parseResult
        print resultExpr


--------------------------------------------------------------------------------
