module Main where

import Scanner
import CST

main::IO()
main =
    do
    let text = "( + x (- x))"
    let tkns = tokenize text
    let cst = expr tkns
    print tkns
    print cst
