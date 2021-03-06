module Primes where

primes :: [Int]
primes = sieve [2 .. 10000]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:rest) = p : sieve (f rest) 
    -- where f = filter (\x -> (x `mod` p) > 0)
    where f = filter (not . (== 0) . (`mod` p))     -- so-called "point free" style

isPrime :: Int -> Maybe Bool
isPrime n | n < 2 = Nothing
  | n >= length primes = Nothing
  | otherwise = Just (n `elem` primes)


unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next:primes) = if n `mod` next == 0
                                        then next:unsafePrimeFactors (n `div` next) (next:primes)
                                        else unsafePrimeFactors n primes

primeFactors :: Int -> Maybe [Int]
primeFactors n | n < 2 = Nothing
               | n >= length primes = Nothing
               | otherwise = Just (unsafePrimeFactors n primesLessThanN)
                   where primesLessThanN = filter (<= n) primes

