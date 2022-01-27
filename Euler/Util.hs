{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Euler.Util where

import Data.List (union)
import qualified Data.Set as PQ
import GHC.Base (build)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

-- primes :: [Integer]
-- primes = sieve [2 ..] where sieve l = if null l then [] else p : sieve [x | x <- tail l, x `mod` p /= 0] where p = head l

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

primes :: [Integer]
primes = 2 : sieve [3, 5 ..]
  where
    sieve (x : xs) = x : sieve' xs (insertprime x xs PQ.empty)

    sieve' (x : xs) table
      | nextComposite == x = sieve' xs (adjust x table)
      | otherwise = x : sieve' xs (insertprime x xs table)
      where
        (nextComposite, _) = PQ.findMin table

    adjust x table
      | n == x = adjust x (PQ.insert (n', ns) newPQ)
      | otherwise = table
      where
        Just ((n, n' : ns), newPQ) = PQ.minView table

    insertprime p xs = PQ.insert (p * p, map (* p) xs)

primesBelow :: Integer -> [Integer]
primesBelow n = helper primes where helper l = if h >= n then [] else h : helper (tail l) where h = head l

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

isPrime :: Integer -> Bool
isPrime n = (n > 1) && null [x | x <- [2 .. isqrt n], n `mod` x == 0]

listUntil :: (a -> Bool) -> [a] -> [a]
listUntil _ [] = []
listUntil p lst = if p h then [] else h : listUntil p (tail lst) where h = head lst

triangle :: Integer -> Integer
triangle n = n * (n + 1) `div` 2

isTriangle :: Integer -> Bool
isTriangle t = t > 0 && t == triangle ((isqrt (8 * t + 1) - 1) `div` 2)

square :: Integer -> Integer
square n = n * n

isSquare :: Integer -> Bool
isSquare n = n == square (isqrt n)

pentagonal :: Integer -> Integer
pentagonal n = n * (3 * n - 1) `div` 2

isPentagonal :: Integer -> Bool
isPentagonal p = p > 0 && p == pentagonal ((1 + isqrt (24 * p + 1)) `div` 6)

hexagonal :: Integer -> Integer
hexagonal n = n * (2 * n - 1)

isHexagonal :: Integer -> Bool
isHexagonal h = h > 0 && h == hexagonal ((isqrt (8 * h + 1) + 1) `div` 4)

heptagonal :: Integer -> Integer
heptagonal n = n * (5 * n - 3) `div` 2

isHeptagonal :: Integer -> Bool
isHeptagonal n = n == heptagonal ((3 + isqrt (40 * n + 9)) `div` 10)

octagonal :: Integer -> Integer
octagonal n = n * (3 * n - 2)

isOctagonal :: Integer -> Bool
isOctagonal n = n == octagonal ((1 + isqrt (3 * n + 1)) `div` 3)

powersOf :: Integer -> Integer -> Integer
powersOf n k = if n `mod` k == 0 then 1 + powersOf (n `div` k) k else 0

primeFactorization :: Integer -> [(Integer, Integer)]
primeFactorization 1 = []
primeFactorization n = do
  let ps = filter (\p -> n `mod` p == 0) (takeWhile (<= isqrt n + 1) primes) -- primes dividing n
  if null ps
    then [(n, 1)] -- if no prime divides n, n must be prime
    else let p = head ps; k = powersOf n p in (p, k) : primeFactorization (n `div` (p ^ k))

eulerPhi :: Integer -> Integer
eulerPhi n = product [(p - 1) * p ^ (k - 1) | (p, k) <- primeFactorization n]

-- Read file contents
fileGetContents :: String -> IO String
fileGetContents path = do
  handle <- openFile path ReadMode
  hGetContents handle

-- Split function
splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy delimiterChar inputString = foldr f [""] inputString
  where
    f :: Char -> [String] -> [String]
    f currentChar allStrings@(partialString : handledStrings)
      | currentChar == delimiterChar = "" : allStrings -- start a new partial string at the head of the list of all strings
      | otherwise = (currentChar : partialString) : handledStrings -- add the current char to the partial string

-- Divisors (technically should omit sqrt(n) if n is a square..)
divisors :: Integer -> [Integer]
divisors n = concat [[k, n `div` k] | k <- [1 .. isqrt n], n `mod` k == 0]

properDivisors :: Integer -> [Integer]
properDivisors n = 1 : concat [[k, n `div` k] | k <- [2 .. isqrt n], n `mod` k == 0]

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n