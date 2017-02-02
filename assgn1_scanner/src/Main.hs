module Main where

import Scanner

main::IO()
main =
  do
  let text = "(+ (+ x 12) (- y))"
  let tkns = tokenize text
  let valid = isValidExprTokens tkns
  print tkns
  print valid
