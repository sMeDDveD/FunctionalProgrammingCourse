{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Char ( digitToInt )
import Data.Map ( Map, fromList, (!?) )
import Data.Maybe ( isJust, fromJust )

compose = Prelude.foldr (.) id

binop :: (a -> a -> a) -> [a] -> [a]
binop op (x : y : zs) = op y x : zs

unop :: (a -> a) -> [a] -> [a]
unop op (x : ys) = op x : ys

binops :: Map String (Int -> Int -> Int)
binops =
  fromList
    [ 
      ("+", (+)),
      ("-", (-)),
      ("*", (*)),
      ("/", div),
      ("%", mod)
    ]

unops :: Map String (Int -> Int)
unops =
  fromList
    [
      ("~", negate)
    ]

tryDecodeBinop :: String -> Maybe ([Int] -> [Int])
tryDecodeBinop s = fmap binop (binops !? s)

tryDecodeUnop :: String -> Maybe ([Int] -> [Int])
tryDecodeUnop s = fmap unop (unops !? s)

tryDecodeDigit :: String -> Maybe ([Int] -> [Int])
tryDecodeDigit [c] | '0' <= c && c <= '9' = Just(unop f) where f x = x * 10 + digitToInt c


decoders = [tryDecodeBinop, tryDecodeUnop, tryDecodeDigit]

firstJusts [] = Nothing
firstJusts (xs:x) = if isJust xs then xs else firstJusts x

imap fs x = map (\f -> f x) fs

decode :: String -> [Int] -> [Int]
decode = fromJust . firstJusts . imap decoders

run :: String -> [Int] -> [Int]
run = compose . map decode . reverse . words
