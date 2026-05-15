module Solution where

import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import Data.Map qualified as Map
import System.IO (hFlush, stdout)

-- State monad

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

-- StateT and "Treasure Hunters" Game Simulation

data Location
  = Start
  | Empty
  | DecisionPoint [(String, Int)] -- (option label, destination position)
  | Obstacle Int                  -- pushes player back by N spaces
  | Treasure Int                  -- awards N bonus points
  | Trap Int                      -- deducts N points
  | Goal                          -- main treasure (end)
  deriving (Show, Eq)

data GameState = GameState
  { gsPosition :: Int
  , gsEnergy   :: Int
  , gsScore    :: Int
  , gsBoard    :: Map Int Location
  } deriving (Show)

type AdventureGame a = StateT GameState IO a

gameBoard :: Map Int Location
gameBoard = Map.fromList
  [ (0,  Start)
  , (1,  Treasure 3)
  , (2,  Empty)
  , (3,  DecisionPoint [("forest path", 4), ("cave path", 8)])
  --  forest path
  , (4,  Obstacle 2)
  , (5,  Treasure 10)
  , (6,  Trap 4)
  , (7,  DecisionPoint [("bridge", 12), ("swamp", 10)])
  --  cave path
  , (8,  Trap 3)
  , (9,  Treasure 15)
  --  swamp detour
  , (10, Obstacle 1)
  , (11, Empty)
  --  convergence
  , (12, Treasure 5)
  , (13, Empty)
  , (14, Goal)
  ]

initialState :: GameState
initialState = GameState
  { gsPosition = 0
  , gsEnergy   = 20
  , gsScore    = 0
  , gsBoard    = gameBoard
  }

-- 4. Player movement and decisions

movePlayer :: Int -> AdventureGame Int
movePlayer diceRoll = do
  st <- get
  let newPos = gsPosition st + diceRoll
  put st { gsPosition = newPos, gsEnergy = gsEnergy st - 1 }
  liftIO $ putStrLn $ "🏃 Moved " ++ show diceRoll ++ " space(s) to position " ++ show newPos
  pure diceRoll

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
  liftIO $ putStrLn "\n🔀 Decision point! Choose your path:"
  liftIO $ getPlayerChoice options

-- 5. Game loop

handleLocation :: AdventureGame Bool
handleLocation = do
  st <- get
  let pos = gsPosition st
      loc = Map.findWithDefault Empty pos (gsBoard st)
  case loc of
    Start -> do
      liftIO $ putStrLn "📍 You are at the starting position."
      pure False
    Empty -> do
      liftIO $ putStrLn "📍 Nothing special here."
      pure False
    DecisionPoint opts -> do
      let labels = map fst opts
      choice <- makeDecision labels
      case lookup choice opts of
        Just dest -> do
          modify (\s -> s { gsPosition = dest })
          liftIO $ putStrLn $ "➡️  Heading to position " ++ show dest
        Nothing -> pure ()
      pure False
    Obstacle n -> do
      let newPos = max 0 (gsPosition st - n)
      modify (\s -> s { gsPosition = newPos, gsEnergy = gsEnergy s - n })
      liftIO $ putStrLn $ "🪨 Obstacle! Pushed back " ++ show n ++ " space(s) to position " ++ show newPos
      liftIO $ putStrLn $ "  Lost " ++ show n ++ " extra energy."
      pure False
    Treasure n -> do
      modify (\s -> s { gsScore = gsScore s + n })
      liftIO $ putStrLn $ "💎 Treasure! Gained " ++ show n ++ " points."
      pure False
    Trap n -> do
      modify (\s -> s { gsScore = max 0 (gsScore s - n) })
      liftIO $ putStrLn $ "⚠️  Trap! Lost " ++ show n ++ " points."
      pure False
    Goal -> do
      liftIO $ putStrLn "🏆 You found the main treasure! You win!"
      pure True

playTurn :: AdventureGame Bool
playTurn = do
  st <- get
  liftIO $ displayGameState st
  if gsEnergy st <= 0
    then do
      liftIO $ putStrLn "💀 You ran out of energy! Game over."
      pure True
    else do
      roll <- liftIO getDiceRoll
      _ <- movePlayer roll
      handleLocation

playGame :: AdventureGame ()
playGame = do
  liftIO $ putStrLn "\n🗺️  === TREASURE HUNTERS ==="
  liftIO $ putStrLn "Find the treasure before your energy runs out!\n"
  go
  where
    go = do
      gameOver <- playTurn
      if gameOver
        then do
          st <- get
          liftIO $ putStrLn $ "\n🏁 Final score: " ++ show (gsScore st)
        else go

-- 6. User interaction in IO

getDiceRoll :: IO Int
getDiceRoll = do
  putStr "\n🎲 Enter dice roll (1-6): "
  hFlush stdout
  input <- getLine
  case reads input :: [(Int, String)] of
    [(n, "")] | n >= 1 && n <= 6 -> pure n
    _ -> do
      putStrLn "Invalid input. Please enter a number between 1 and 6."
      getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState gs = do
  putStrLn "──────────────────────────────"
  putStrLn $ "  📍 Position : " ++ show (gsPosition gs)
  putStrLn $ "  ⚡ Energy   : " ++ show (gsEnergy gs)
  putStrLn $ "  💰 Score    : " ++ show (gsScore gs)
  putStrLn "──────────────────────────────"

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
  mapM_ (\(i, opt) -> putStrLn $ "  " ++ show i ++ ") " ++ opt)
        (zip [1 :: Int ..] options)
  putStr "Your choice: "
  hFlush stdout
  input <- getLine
  case reads input :: [(Int, String)] of
    [(idx, "")] | idx >= 1 && idx <= length options ->
      pure (options !! (idx - 1))
    _ -> do
      putStrLn "Invalid choice, try again."
      getPlayerChoice options