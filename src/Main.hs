-- | Main entry point to the application.
module Main where

import Test.QuickCheck
import Data.List

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

prop_idempotent :: [a] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

-- | The main entry point.
main :: IO ()
main = do
    let r = prop_idempotent [1,5,2,1,2,0,9]
    putStr $ show r
