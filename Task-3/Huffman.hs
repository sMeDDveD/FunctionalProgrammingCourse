{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Huffman
  ( WeightedTree,
    encodeHuffman,
    decodeHuffman,
    buildHuffmanTree,
    buildEncodings,
  )
where

import Data.Function (on)
import Data.List (insertBy)
import Data.Map (Map, assocs, singleton, union, (!))
import Hist (Histogram)

data Tree a = Branch (Tree a) (Tree a) | Leaf a

type WeightedTree a = Tree (a, Int)

getWeight :: WeightedTree a -> Int
getWeight (Branch a b) = getWeight a + getWeight b
getWeight (Leaf (v, w)) = w

type HuffmanEncodings a = Map a [Bool]

buildHuffmanTree :: Ord a => Histogram a -> WeightedTree a
buildHuffmanTree frequencies
  | null frequencies = error "Non-empty frequencies expected"
  | otherwise = mergeHuffmanTrees basis
  where
    basis = map Leaf (assocs frequencies)

mergeHuffmanTrees :: Ord a => [WeightedTree a] -> WeightedTree a
mergeHuffmanTrees [tree] = tree
mergeHuffmanTrees (lhs : rhs : others) =
  let n = Branch lhs rhs
   in mergeHuffmanTrees $ insertBy (compare `on` getWeight) n others

buildEncodings :: Ord a => WeightedTree a -> HuffmanEncodings a
buildEncodings = helper []
  where
    helper prefix (Leaf (v, w)) = singleton v (reverse prefix)
    helper prefix (Branch lhs rhs) =
      helper (True : prefix) lhs `union` helper (False : prefix) rhs

encodeHuffman :: (Foldable t, Ord a) => HuffmanEncodings a -> t a -> [Bool]
encodeHuffman encodings = concatMap (encodings !)

decodeHuffmanBlock :: WeightedTree a -> [Bool] -> (a, [Bool])
decodeHuffmanBlock (Leaf (v, _)) codes = (v, codes)
decodeHuffmanBlock (Branch a b) (c : codes) =
  decodeHuffmanBlock (if c then a else b) codes

decodeHuffman :: WeightedTree a -> [Bool] -> [a]
decodeHuffman tree = reverse . decoder []
  where
    decoder d [] = d
    decoder d codes =
      let (curr, r) = decodeHuffmanBlock tree codes
       in decoder (curr : d) r