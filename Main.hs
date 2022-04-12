-- Aula 1 ------------------------------------

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)


-- Aula 2 ------------------------------------

xor :: Bool -> Bool -> Bool
xor x y = x /= y

existsPositive :: [Int] -> Bool
existsPositive [] = False
existsPositive (x : xs) 
              | x <= 0    = existsPositive xs
              | otherwise = True

-- Aula 3 ------------------------------------

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

