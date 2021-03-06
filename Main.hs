{-# LANGUAGE InstanceSigs #-}
{-
-- import Data.Char

-- Aula 1 ---------------------------------------

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)


-- Aula 2 ---------------------------------------

xor :: Bool -> Bool -> Bool
xor x y = x /= y

existsPositive :: [Int] -> Bool
existsPositive [] = False
existsPositive (x : xs) 
              | x <= 0    = existsPositive xs
              | otherwise = True

-- Aula 3 ---------------------------------------

-- bools [bool] ===>>> [True]

-- nums [[Int]] ===>>> [[1, 2], [3, 4]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f y = f y

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x) 

-- Aula 4 ---------------------------------------

minList :: Ord a => [a] -> a
minList []           = error "empty list."
minList (x : [])     = x
minList (x : y : xs)
 | x < y             = minList (x : xs)
 | y < x             = minList (y : xs)
 | otherwise         = minList (y : xs)

andList :: [Bool] -> Bool
andList []       = True
andList (x : xs) = x && andList xs

orList :: [Bool] -> Bool
orList []       = False
orList (x : xs) = x || orList xs

indexOf :: Int -> [Int] -> Int
indexOf _ []       = -1
indexOf a (x : xs) = indexOfAc a (x : xs) 0
 where
  indexOfAc _ [] _ = -1
  indexOfAc a (x : xs) ac
   | a == x = (ac + 1)
   | otherwise = indexOfAc a xs (ac + 1)

removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll a (x : xs)
 | a == x = removeAll a xs
 | otherwise = x : removeAll a xs

-- Aula 5 ---------------------------------------

-- capitalize :: String -> String
-- capitalize xs = map toUpper xs


withoutPrimes :: [Int] -> [Int]
withoutPrimes xs = filter (not . isPrime) xs
 where
  isPrime x = length (factors x) == 2
  factors x = [x | y <- [1 .. x], x `mod` y == 0]
 
-- Aula 6 ---------------------------------------

takeWhileJ :: (a -> Bool) -> [a] -> [a]
takeWhileJ _ [] = []
takeWhileJ p (x: xs)
 | p x = x : takeWhileJ p xs
 | otherwise = []

takeWhileFoldr :: (a -> Bool) -> [a] -> [a]
takeWhileFoldr f xs = foldr step base xs
 where
  step x ac
   | f x            = x : ac
   | otherwise      = ac
  base              = []

allJ :: (a -> Bool) -> [a] -> Bool
allJ _ []         = True
allJ f (x : xs)
 | f x            = True && allJ f xs
 | otherwise      = False && allJ f xs

allFoldr :: (a -> Bool) -> [a] -> Bool
allFoldr f xs    = foldr step base xs
 where
  step x ac
   | f x         = True && ac
   | otherwise   = False
  base           = True

concatMapJ :: (a -> [b]) -> [a] -> [b]
concatMapJ _ [] = []
concatMapJ f (x : xs) = f x ++ concatMap f xs

concatMapFoldr :: (a -> [b]) -> [a] -> [b]
concatMapFoldr f xs = foldr step base xs
 where
  step x ac = f x ++ ac
  base = []

-- Aula 7 ---------------------------------------

data Shape
     = Rectangle Float Float
     | Circle Float
     | Triangle Float Float

area :: Shape -> Float
area (Rectangle b h) = b * h
area (Circle r) = 3.14 * r * r
area (Triangle b h) = (b * h) / 2

data IntTree = IntLeaf | IntNode Int IntTree IntTree

countLeafNode :: IntTree -> (Int, Int)
countLeafNode t = (countNodeAc t , countLeafAc t)
 where
  countLeafAc IntLeaf = 1
  countLeafAc (IntNode _ l r) = countLeafAc l + countLeafAc r
  countNodeAc IntLeaf = 0
  countNodeAc (IntNode _ l r) = 1 + countNodeAc l + countNodeAc r

type Name = String
type Surname = String
type SendOffer = Bool

data Client = Client { name :: Name,
                       surname :: Surname,
                       offers :: SendOffer
                     }

offering :: [Client] -> [Client]
offering xs = filter (n) xs
 where
  n (Client name surname offers) = offers 
-}
-- Aula 8 ---------------------------------------

data Vec3 = Vec3 Float Float Float
instance Eq Vec3 where
         (==) (Vec3 x y z) (Vec3 x' y' z') 
              | x == x' && y == y' && z == z' = True
              | otherwise = False
 
data Person = 
     Person {
             name :: String,
             age :: Int
     }
instance Eq Person where
         (==) (Person name age) (Person name' age')
              | name == name' = True
              | otherwise = False

instance Show Person where
         show (Person name age) = name












