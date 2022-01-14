module Hist
  ( Histogram,
    buildHistogram,
    normalizeHistogram,
  )
where

import Data.Map (Map, fromListWith, map)
import Prelude hiding (map)

type Histogram a = Map a Int

buildHistogram :: (Ord k) => [k] -> Histogram k
buildHistogram elements = fromListWith (+) pairs
  where
    pairs = zip elements (repeat 1)

toInt :: Double -> Int
toInt = round

toDouble :: Int -> Double
toDouble = fromIntegral

normalizeHistogram :: Int -> Histogram k -> Histogram k
normalizeHistogram h m =
  map (\x -> toInt (norm * toDouble x)) m
  where
    norm = toDouble h / toDouble (maximum m)
