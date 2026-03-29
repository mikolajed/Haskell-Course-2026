module Solution where

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
  deriving (Show, Eq)

-- 1. Functor for Sequence
instance Functor Sequence where
  fmap :: (a -> b) -> Sequence a -> Sequence b
  fmap _ Empty = Empty
  fmap f (Single a) = Single (f a)
  fmap f (Append l r) = Append (fmap f l) (fmap f r)

-- 2. Foldable for Sequence
instance Foldable Sequence where
  foldMap :: (Monoid m) => (a -> m) -> Sequence a -> m
  foldMap _ Empty = mempty
  foldMap f (Single a) = f a
  foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
seqToList = foldr (:) []

seqLength :: Sequence a -> Int
seqLength = length

-- 3. Semigroup and Monoid for Sequence
instance Semigroup (Sequence a) where
  (<>) :: Sequence a -> Sequence a -> Sequence a
  (<>) Empty r = r
  (<>) l Empty = l
  (<>) (Single a) r = Append (Single a) r
  (<>) l (Single a) = Append l (Single a)
  (<>) (Append l1 r1) (Append l2 r2) = Append l1 (Append r1 (Append l2 r2))

instance Monoid (Sequence a) where
  mempty :: Sequence a
  mempty = Empty

-- 4. Tail Recursion and Sequence Search
tailElem :: (Eq a) => a -> Sequence a -> Bool
tailElem target initialSeq = go [initialSeq]
  where
    go [] = False
    go (Empty : tail) = go tail
    go (Single x : tail)
      | x == target = True
      | otherwise = go tail
    go (Append l r : tail) = go (l : r : tail)

-- 5. Tail Recursion and Sequence Flatten
tailToList :: Sequence a -> [a]
tailToList initialSeq = go [initialSeq] []
  where
    go [] acc = acc
    go (Empty : tail) acc = go tail acc
    go (Single x : tail) acc = go tail (x : acc)
    go (Append l r : tail) acc = go (r : l : tail) acc

-- 5. Tail Recursion and Reverse Polish Notation
data Token = TNum Int | TAdd | TSub | TMul | TDiv

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [x] = Just x
    go [] _ = Nothing
    go (TNum n : ts) stack = go ts (n : stack)
    go (TAdd : ts) (y : x : stack) = go ts ((x + y) : stack)
    go (TSub : ts) (y : x : stack) = go ts ((x - y) : stack)
    go (TMul : ts) (y : x : stack) = go ts ((x * y) : stack)
    go (TDiv : ts) (y : x : stack)
      | y == 0 = Nothing
      | otherwise = go ts ((x `div` y) : stack)
    go _ _ = Nothing

-- 6. Expressing functions via `foldr` and `foldl`

-- (a) myReverse :: [a] -> [a]
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

-- (b) myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x acc -> if p x then x : acc else []) []

-- (c) decimal :: [Int] -> Int
decimal :: [Int] -> Int
decimal = foldl (\acc x -> acc * 10 + x) 0

-- 7. Run-length encoding via folds

-- (a) encode :: Eq a => [a] -> [(a, Int)]
encode :: (Eq a) => [a] -> [(a, Int)]
encode = foldr f []
  where
    f x [] = [(x, 1)]
    f x ((y, n) : acc)
      | x == y = (y, n + 1) : acc
      | otherwise = (x, 1) : (y, n) : acc

-- (b) decode :: [(a, Int)] -> [a]
decode :: [(a, Int)] -> [a]
decode = foldr (\(x, n) acc -> replicate n x ++ acc) []
