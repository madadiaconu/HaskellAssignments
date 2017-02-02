{-# LANGUAGE FlexibleInstances #-}

module Parser where

--- Parser a ------------------------------------------------------------------

type Parser a = [String] -> (Maybe a, [String])

anyToken :: Parser String
anyToken (tkn:tkns)          = (Just tkn, tkns)
anyToken []                  = (Nothing, [])

-- combinators ----------------------------------------------------------------

-- result --------

result :: a -> Parser a
result x = \tkns -> (Just x, tkns)

-- andThen ----------

andThen  :: Parser a -> (a -> Parser b) -> Parser b
andThen p fn =
    \tkns ->
        case p tkns of
            (Nothing, _)    -> (Nothing, tkns)
            (Just a, rest)  -> (fn a) rest


oneToken :: Parser String
oneToken = anyToken `andThen` \tkn -> result tkn


twoTokens' :: Parser String
twoTokens' = anyToken `andThen` (\tkn1 -> anyToken)


twoTokens  :: Parser String
twoTokens  = anyToken `andThen`
                       (\w1 -> anyToken
                            `andThen`
                                (\w2 -> result (w1 ++ w2))
                       )

twoTokenList  :: Parser [String]
twoTokenList  = anyToken `andThen`
                      (\w1 -> anyToken
                            `andThen`
                                (\w2 -> result [w1,w2])
                      )


parseTokensRetRight :: Parser String
parseTokensRetRight = anyToken `andThen` \_ -> anyToken

-- right ----------

andThenRetRight :: Parser a -> Parser b -> Parser b
andThenRetRight l r =  l  `andThen`  \_ -> r

-- left -----------

andThenRetLeft :: Parser a -> Parser b -> Parser a
andThenRetLeft l r = l `andThen`
               \x -> r `andThen`
                   \_ -> result x


twoTokensInBrackets :: Parser [String]
twoTokensInBrackets = (token "(") `andThenRetRight`
                           twoTokenList `andThenRetLeft` (token ")")


twoRes = twoTokensInBrackets ["(","A","B",")"]

-- mapping and filtering -----------------------------------------------------

mapTo :: Parser a -> (a -> b) -> Parser b
mapTo p fn  = p `andThen` \a -> result (fn a)


anyTokenLength :: Parser Int
anyTokenLength = anyToken `mapTo` (\tkn -> length tkn)

twoTokensConcatenated :: Parser String
twoTokensConcatenated = twoTokenList `mapTo` (\twoTkns -> concat  twoTkns)



filterBy :: Parser a -> (a -> Bool) -> Parser a
filterBy p pred =
    \tkns ->
        case p tkns of
            (Nothing, _)    -> (Nothing, tkns)
            (Just x, rest)  -> if pred x then (Just x, rest)
                               else (Nothing, tkns)

token :: String -> Parser String
token tkn =  anyToken `filterBy` (== tkn)


open :: Parser String
open = token "("


close :: Parser String
close = token ")"


word :: Parser String
word = anyToken `filterBy` isWord
            where isWord tkn = tkn /= "(" && tkn /= ")"


-- further combinators --------------------------------------------------------

-- multiple -----------

multiple :: Parser a -> Parser [a]
multiple p =
    \tkns ->
       let
          (mbP, pRest) = p tkns
       in
          case mbP of
              Nothing  -> (Just [], pRest)
              Just pR  ->
                 case multiple p pRest of
                    (Just results, rest)  -> (Just (pR:results), rest)
                    (Nothing, rest)       -> (Nothing, rest) -- should never occur


multipleAs :: Parser [String]
multipleAs = multiple (token "A")

threeAs :: (Maybe [String], [String])
threeAs = multipleAs ["A", "A", "A", "B", "C"]

noA :: (Maybe [String], [String])
noA = multipleAs ["B", "C"]

-- or ------------

orElse :: Parser a -> Parser a -> Parser a
orElse p q =
    \tkns ->
        let
           (pR, pRest) = p tkns
        in
           case pR of
              Nothing  -> q tkns
              Just _   -> (pR, pRest)


aOrB :: Parser String
aOrB = (token "A") `orElse` (token "B")


(parse1, rest1) =  aOrB ["A", "B", "C"]
(parse2, rest2) =  aOrB rest1
(noParseB, rest3) =  aOrB rest2


multipleAOrBs :: Parser [String]
multipleAOrBs = multiple ((token "A") `orElse` (token "B"))

(parse4, rest4) = multipleAOrBs ["A", "B", "B", "A", "C", "D"]


multipleAOrBsInBrackets :: Parser [String]
multipleAOrBsInBrackets = open `andThenRetRight` multipleAOrBs `andThenRetLeft` close

(parse5, rest5) = multipleAOrBsInBrackets ["(","A", "B", "B", "A", ")"]

-------------------------------------------------------------------------------
