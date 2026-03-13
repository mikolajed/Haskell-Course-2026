{-# LANGUAGE BangPatterns #-}

module Solution where

-- ============================================================================
-- List Comprehensions
-- ============================================================================

-- 1. Goldbach Pairs
goldbachPairs :: Int -> [(Int, Int)]
-- We only need to check up to n `div` 2 because if p > n `div` 2, then q < n `div` 2,
-- and we would have already found the pair (q, p) when checking p = q,
-- otherwise we would have duplicates.
goldbachPairs n = [(p, q) | p <- primesTo (n `div` 2), let q = n - p, isPrime q]

-- 2. Coprime Pairs
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs = [(x, y) | x <- us, y <- us, x < y, gcd x y == 1]
  where
    us = unique xs

-- Helper to remove duplicates from a list
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique [y | y <- xs, y /= x]

-- 3. Sieve of Eratosthenes
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2 .. n]

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = n `elem` primesTo n

-- 4. Matrix Multiplication
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul [] _ = []
matMul _ [] = []
matMul a@(rowA : _) b@(rowB : _) =
  [[cell i j | j <- [0 .. n - 1]] | i <- [0 .. m - 1]]
  where
    m = length a
    p = length rowA
    n = length rowB
    cell i j = sum [a !! i !! k * b !! k !! j | k <- [0 .. p - 1]]

-- 5. Permutations
permutations :: (Eq a) => Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations k xs =
  [ x : p
  | x <- xs,
    p <- permutations (k - 1) [y | y <- xs, y /= x]
  ]

-- ============================================================================
-- Lazy/Eager Evaluation, seq, and Bang Patterns
-- ============================================================================

-- 6. Hamming Numbers
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | x > y = y : merge (x : xs) ys
  | otherwise = x : merge xs ys

hamming :: [Integer]
hamming = 1 : merge (map (* 2) hamming) (merge (map (* 3) hamming) (map (* 5) hamming))

-- 7. Integer Power with Bang Patterns
power :: Int -> Int -> Int
power b e = go b e 1
  where
    go _ 0 !acc = acc
    go b e !acc = go b (e - 1) (acc * b)

-- 8. Running Maximum: `seq` vs. Bang Patterns

-- Version 1: Using `seq`
listMaxSeq :: [Int] -> Int
listMaxSeq (x : xs) = go xs x
  where
    go [] acc = acc
    go (y : ys) acc =
      let newAcc = max y acc
       in newAcc `seq` go ys newAcc

-- Version 2: Using Bang Patterns
listMaxBang :: [Int] -> Int
listMaxBang (x : xs) = go xs x
  where
    go [] !acc = acc
    go (y : ys) !acc = go ys (max y acc)

-- 9. Infinite Prime Stream
primes :: [Int]
primes = sieve [2 ..]

isPrime' :: Int -> Bool
isPrime' n
  | n < 2 = False
  | otherwise = let (p : _) = [x | x <- primes, x >= n] in p == n

-- 10. Strict Accumulation and Space Leak

-- (a) No strictness annotations
mean :: [Double] -> Double
mean xs =
  let (sum, count) = go xs (0, 0)
   in sum / count
  where
    go [] acc = acc
    go (y : ys) (sum, count) = go ys (sum + y, count + 1)

-- (b) Fix space leak using bang patterns
meanBang :: [Double] -> Double
meanBang xs =
  let (sum, count) = go xs 0 0
   in sum / count
  where
    go [] !sum !count = (sum, count)
    go (y : ys) !sum !count = go ys (sum + y) (count + 1)

-- (c) Single pass mean and variance
meanVar :: [Double] -> (Double, Double)
meanVar xs =
  let (sumX, sumX2, n) = go xs 0 0 0
      mu = sumX / n
      var = (sumX2 / n) - (mu * mu)
   in (mu, var)
  where
    go [] !sumX !sumX2 !n = (sumX, sumX2, n)
    go (y : ys) !sumX !sumX2 !n = go ys (sumX + y) (sumX2 + (y * y)) (n + 1)
