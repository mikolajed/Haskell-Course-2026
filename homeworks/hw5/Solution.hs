module Solution where

import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map

-- 1. Stack machine
data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
  deriving (Show, Eq)

execInstr :: Instr -> State [Int] ()
execInstr (PUSH n) = modify (n :)
execInstr POP = do
  s <- get
  case s of
    (_ : rest) -> put rest
    _ -> pure ()
execInstr DUP = do
  s <- get
  case s of
    (x : _) -> modify (x :)
    _ -> pure ()
execInstr SWAP = do
  s <- get
  case s of
    (x : y : rest) -> put (y : x : rest)
    _ -> pure ()
execInstr ADD = do
  s <- get
  case s of
    (x : y : rest) -> put (x + y : rest)
    _ -> pure ()
execInstr MUL = do
  s <- get
  case s of
    (x : y : rest) -> put (x * y : rest)
    _ -> pure ()
execInstr NEG = do
  s <- get
  case s of
    (x : rest) -> put (negate x : rest)
    _ -> pure ()

execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg prog = execState (execProg prog) []

-- 2. Expression evaluator with variable bindings

data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr -- bind the value of the expression to the name, return that value
  | Seq Expr Expr -- evaluate the left, then the right; return the value of the right
  deriving (Show, Eq)

eval :: Expr -> State (Map String Int) Int
eval (Num n) = pure n
eval (Var name) = do
  env <- get
  pure (env Map.! name)
eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  pure (v1 + v2)
eval (Mul e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  pure (v1 * v2)
eval (Neg e) = do
  v <- eval e
  pure (negate v)
eval (Assign name expr) = do
  val <- eval expr
  modify (Map.insert name val)
  pure val
eval (Seq e1 e2) = do
  _ <- eval e1
  eval e2

runEval :: Expr -> Int
runEval expr = evalState (eval expr) Map.empty

-- 3. Memoised edit (Levenshtein) distance

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
  cache <- get
  case Map.lookup (i, j) cache of
    Just d -> pure d
    Nothing -> do
      d <- compute i j
      modify (Map.insert (i, j) d)
      pure d
  where
    compute 0 j' = pure j'
    compute i' 0 = pure i'
    compute i' j'
      | xs !! (i' - 1) == ys !! (j' - 1) = editDistM xs ys (i' - 1) (j' - 1)
      | otherwise = do
          d1 <- editDistM xs ys (i' - 1) j'
          d2 <- editDistM xs ys i' (j' - 1)
          d3 <- editDistM xs ys (i' - 1) (j' - 1)
          pure (1 + minimum [d1, d2, d3])

editDistance :: String -> String -> Int
editDistance xs ys = evalState (editDistM xs ys (length xs) (length ys)) Map.empty
