import Control.Monad (when)
import Data.Function (fix)
import Data.List (elemIndex)
import Data.Maybe (fromJust, isJust)

factorial :: Int -> Integer
factorial 0 = 1
factorial 1 = 1
factorial 2 = 2
factorial 3 = 6
factorial 4 = 24
factorial 5 = 120
factorial 6 = 720
factorial 7 = 5040
factorial 8 = 40320
factorial 9 = 362880
factorial n = 0

nextTerm :: Int -> Int
nextTerm n = fromInteger $ sum (map (\c -> factorial (read [c])) (show n))

chain :: Int -> Int
chain n = helper n []
  where
    helper n lst =
      if n `elem` take 5 lst
        then 0
        else 1 + helper (nextTerm n) (n : lst)

main :: IO ()
main = print $ length $ filter (== 60) $ map chain [2 .. 999999]
