import Data.List (unfoldr)

unfoldToIota :: (Ord b, Num b) => b -> [b]
unfoldToIota n = unfoldr gen 1
  where
    gen x =
      if n > x
        then Just (x, x + 1)
        else Nothing

binGen :: Integral b => b -> Maybe (b, b)
binGen x =
  if x /= 0
    then Just (mod x 2, div x 2)
    else Nothing

unfoldToBin :: Integer -> [Integer]
unfoldToBin = unfoldr binGen

firstDivisor :: Integral t => t -> t -> t
firstDivisor first x
  | first ^ 2 > x = x
  | mod x first == 0 = first
  | otherwise = firstDivisor (first + 1) x

divisorGen :: Integral b => (b, b) -> Maybe (b, (b, b))
divisorGen (start, x) =
  let divisor = firstDivisor start x
   in if x /= 1
        then Just (divisor, (divisor, div x divisor))
        else Nothing

unfoldToDivisors :: Integral a => a -> [a]
unfoldToDivisors x = unfoldr divisorGen (2, x)

unfoldToFib :: (Ord a, Num a) => a -> [a]
unfoldToFib x = unfoldr gen (0, 1)
  where
    gen (prev, curr) =
      if prev < x
        then Just (prev, (curr, prev + curr))
        else Nothing

unfoldToFibUnbounded :: [Integer]
unfoldToFibUnbounded =
  unfoldr
    (\(prev, curr) -> Just (prev, (curr, prev + curr)))
    (0, 1)

isqr :: Integer -> Integer
isqr = floor . sqrt . fromInteger

isPrime :: Integer -> Bool
isPrime k =
  (k > 1)
    && null [x | x <- [2 .. isqr k], mod k x == 0]

unfoldToPrimes :: Integer -> [Integer]
unfoldToPrimes n = unfoldr gen 1
  where
    gen x
      | x >= n = Nothing
      | isPrime x = Just (x, succ x)
      | otherwise = gen $ succ x

unfoldToSievePrimes :: [Integer]
unfoldToSievePrimes = unfoldr gen [2 ..]
  where
    gen [] = Nothing -- To suppress compiler's warnings
    gen (xs : x) = Just (xs, filter (\now -> mod now xs /= 0) x)