module Solution where

-- Reader newtype
newtype Reader r a = Reader { runReader :: r -> a }

-- 1. Functor, Applicative, and Monad instances

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader (\env -> f (ra env))

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure x = Reader (\_ -> x)
  liftA2 :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
  liftA2 f (Reader ra) (Reader rb) = Reader (\env -> f (ra env) (rb env))

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= f = Reader (\env -> runReader (f (ra env)) env)

-- 2. Primitive operations

-- Retrieves the entire environment.
ask :: Reader r r
ask = Reader (\env -> env)

-- Retrieves a value derived from the environment by applying a projection.
asks :: (r -> a) -> Reader r a
asks f = Reader f

-- Runs a subcomputation in a locally modified environment.
local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader ra) = Reader (\env -> ra (f env))

-- 3. A practical example — banking system

data BankConfig = BankConfig
  { interestRate   :: Double
  , transactionFee :: Int
  , minimumBalance :: Int
  } deriving (Show)

data Account = Account
  { accountId :: String
  , balance   :: Int
  } deriving (Show)

-- Computes the interest accrued on the account, based on the configured rate.
calculateInterest :: Account -> Reader BankConfig Int
calculateInterest acc = do
  rate <- asks interestRate
  return (floor (fromIntegral (balance acc) * rate))

-- Deducts the transaction fee from the account and returns the updated account.
applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
  fee <- asks transactionFee
  return acc { balance = balance acc - fee }

-- Checks whether the account balance meets the configured minimum.
checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
  minBal <- asks minimumBalance
  return (balance acc >= minBal)

-- Runs the three operations above on a single account and combines their results.
processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc = do
  updatedAcc <- applyTransactionFee acc
  interest   <- calculateInterest acc
  meetsMin   <- checkMinimumBalance acc
  return (updatedAcc, interest, meetsMin)
