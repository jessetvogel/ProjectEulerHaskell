import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Euler.Util (properDivisors)

chainLength :: Integer -> Int
chainLength n = helper n []
  where
    helper n lst
      | n > 1000000 = -1
      | n `elem` lst = 1 + fromJust (elemIndex n lst)
      | otherwise = helper (sum $ properDivisors n) (n : lst)

chainFrom :: Integer -> [Integer]
chainFrom n = helper n []
  where
    helper n lst
      | n `elem` lst = take (1 + fromJust (elemIndex n lst)) lst
      | otherwise = helper (sum $ properDivisors n) (n : lst)

main :: IO ()
main = do
  -- Find maximum chain length
  let (l, n) = maximum [(chainLength n, n) | n <- [1 .. 10000]]
  -- Take the minimum number in that chain
  print $ minimum $ chainFrom n
