module Lib
    ( someFunc
    , inRange
    ) where

inRange :: Int -> Int -> [Int] -> [Int]
inRange _ _ [] = []
inRange a b (x : xs)
                    | a >= b = []
                    | otherwise = x : (inRange (a+1) b xs)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
