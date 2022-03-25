import Data.Function.Memoize (memoize)
import Data.List (sort)
import GHC.Float (rationalToDouble)

cutting :: Int -> [Int]
cutting 1 = [2, 3, 4, 5]
cutting 2 = [3, 4, 5]
cutting 3 = [4, 5]
cutting 4 = [5]
cutting _ = []

expectation :: [Int] -> Rational
expectation = memoize helper
  where
    helper [] = 0
    helper envelope = x + sum (map expectation subenvelopes) / toRational n
      where
        n = length envelope
        x = if n == 1 then 1 else 0
        subenvelopes = [sort $ cutting (envelope !! i) ++ take i envelope ++ drop (i + 1) envelope | i <- [0 .. n - 1]]

main :: IO ()
main = print $ fromRational $ expectation [1]
