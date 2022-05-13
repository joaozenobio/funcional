import Data.Char
import Test.Tasty
import Test.Tasty.HUnit as TH
import Test.Tasty.QuickCheck as TQ
import Test.QuickCheck
import Lib

inRangeUnit :: TestTree
inRangeUnit = testCase "inRage test" (s @?= [2,3,4,5])
              where s = inRange 2 5 [1..10]

correctLength :: Int -> Int -> [Int] -> Bool
correctLength _ _ [] = True
correctLength a b xs = (b - a) >= (length xs) 

correctItems :: [Int] -> [Int] -> Bool
correctItems _  [] = True
correctItems [] _  = True
correctItems (x : xs) ys = elem x ys && (correctItems xs ys)

inRangeProperty :: Int -> Int -> [Int] -> Bool
inRangeProperty a b xs = correctLength a b result && correctItems result xs
                         where result = inRange a b xs

main :: IO ()
main = quickCheck inRangeProperty
