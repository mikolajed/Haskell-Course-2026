module Solution where

import Control.Monad (guard)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Map (Map)
import Data.Map qualified as Map

--------------------------------------------------------------------------------
-- 1. Maze navigation
--------------------------------------------------------------------------------

type Pos = (Int, Int)

data Dir = N | S | E | W deriving (Eq, Ord, Show)

type Maze = Map Pos (Map Dir Pos)

-- (a)
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = Map.lookup pos maze >>= Map.lookup dir

-- (b)
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze start dirs = foldl step (Just start) dirs
  where
    step mpos dir = mpos >>= \pos -> move maze pos dir

-- (c)
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze start dirs = do
  (_, path) <- foldl step (Just (start, [start])) dirs
  return path
  where
    step acc dir = do
      (pos, path) <- acc
      next <- move maze pos dir
      return (next, path ++ [next])

--------------------------------------------------------------------------------
-- 2. Decoding a message
--------------------------------------------------------------------------------

type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (\c -> Map.lookup c key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)

--------------------------------------------------------------------------------
-- 3. Seating arrangements
--------------------------------------------------------------------------------

type Guest = String

type Conflict = (Guest, Guest)

perms :: (Eq a) => [a] -> [[a]]
perms [] = [[]]
perms xs = do
  x <- xs
  rest <- perms (filter (/= x) xs)
  return (x : rest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
  (first : rest) <- perms guests
  let perm = first : rest
  let pairs = zip perm (rest ++ [first])
  guard (all (\(a, b) -> (a, b) `notElem` conflicts && (b, a) `notElem` conflicts) pairs)
  return perm

--------------------------------------------------------------------------------
-- 4. Result monad with warnings
--------------------------------------------------------------------------------

data Result a = Failure String | Success a [String]
  deriving (Show, Eq)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap _ (Failure msg) = Failure msg
  fmap f (Success val ws) = Success (f val) ws

instance Applicative Result where
  pure :: a -> Result a
  pure x = Success x []

  (<*>) :: Result (a -> b) -> Result a -> Result b
  Failure msg <*> _ = Failure msg
  _ <*> Failure msg = Failure msg
  Success f ws1 <*> Success x ws2 = Success (f x) (ws1 ++ ws2)

instance Monad Result where
  (>>=) :: Result a -> (a -> Result b) -> Result b
  Failure msg >>= _ = Failure msg
  Success x ws >>= f = case f x of
    Failure msg -> Failure msg
    Success y ws2 -> Success y (ws ++ ws2)

-- (b)

warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

-- (c)
validateAge :: Int -> Result Int
validateAge age
  | age < 0 = failure "Age cannot be negative"
  | age > 150 = do
      warn "Age is above 150"
      return age
  | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

--------------------------------------------------------------------------------
-- 5. Evaluator with simplification log
--------------------------------------------------------------------------------

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr
  deriving (Show, Eq)

-- Apply algebraic simplification rules bottom-up and log each rule applied.
-- First simplify subtrees, then apply rules to the root.
simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)
simplify (Add l r) = do
  l' <- simplify l
  r' <- simplify r
  simplifyAdd l' r'
simplify (Mul l r) = do
  l' <- simplify l
  r' <- simplify r
  simplifyMul l' r'
simplify (Neg e) = do
  e' <- simplify e
  simplifyNeg e'

-- Simplification rules for Add
simplifyAdd :: Expr -> Expr -> Writer [String] Expr
simplifyAdd (Lit 0) e = do
  tell ["Add identity: 0 + e -> e"]
  return e
simplifyAdd e (Lit 0) = do
  tell ["Add identity: e + 0 -> e"]
  return e
simplifyAdd (Lit a) (Lit b) = do
  tell ["Constant folding: " ++ show a ++ " + " ++ show b ++ " -> " ++ show (a + b)]
  return (Lit (a + b))
simplifyAdd l r = return (Add l r)

-- Simplification rules for Mul
simplifyMul :: Expr -> Expr -> Writer [String] Expr
simplifyMul (Lit 0) _ = do
  tell ["Zero absorption: 0 * e -> 0"]
  return (Lit 0)
simplifyMul _ (Lit 0) = do
  tell ["Zero absorption: e * 0 -> 0"]
  return (Lit 0)
simplifyMul (Lit 1) e = do
  tell ["Mul identity: 1 * e -> e"]
  return e
simplifyMul e (Lit 1) = do
  tell ["Mul identity: e * 1 -> e"]
  return e
simplifyMul (Lit a) (Lit b) = do
  tell ["Constant folding: " ++ show a ++ " * " ++ show b ++ " -> " ++ show (a * b)]
  return (Lit (a * b))
simplifyMul l r = return (Mul l r)

-- Simplification rules for Neg
simplifyNeg :: Expr -> Writer [String] Expr
simplifyNeg (Neg e) = do
  tell ["Double negation: --e -> e"]
  return e
simplifyNeg e = return (Neg e)
