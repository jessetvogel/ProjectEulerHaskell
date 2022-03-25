import Control.Monad (when)
import Data.Foldable (for_)
import Data.List (elemIndex, isSuffixOf)
import Data.Maybe (fromJust)
import Data.Ratio (denominator, numerator)
import Euler.Util (divisors, eulerPhi)

-- Conjecture: all cyclic numbers are a decimal expansion of 1/N for some N
-- 1/0.000000001370 = 729927007 (max)
-- 1/0.000000001379 = 725163162 (min)


isCyclic :: Integer -> Bool
isCyclic n = 


-- Gives n digits of q starting at the k'th decimal
digits :: Rational -> Int -> Int -> Integer
digits q k n = floor (q * 10 ^ (k + n)) `mod` (10 ^ n)

skipWhile :: (a -> Bool) -> [a] -> [a]
skipWhile f lst = if f (head lst) then skipWhile f (tail lst) else lst

cycle' :: Rational -> Integer
cycle' q =
  let m = numerator q
      n = denominator q
      exp = expansion (m, n)
      p = period (drop 20 exp)
      digits = take p (skipWhile (== 0) (map (uncurry div) exp))
   in read (concatMap show digits)

expansion :: (Integer, Integer) -> [(Integer, Integer)]
expansion (m, n) = (m, n) : expansion (10 * (m `mod` n), n)

period :: Eq a => [a] -> Int
period list = 1 + fromJust (elemIndex (head list) (tail list))

main :: IO ()
main = do
  for_
    [725163162 .. 729927007]
    ( \n -> do
        let lastDigits = digits (1 / toRational n) (period' n - 5) 5
        putStrLn $ "Last 5 repeating digits of 1 / " ++ show n ++ " are " ++ show lastDigits

        when (lastDigits == 56789) $ print "Done!"
    )

-- main = print $ period' 725163163

-- Period of decimal expansion of 1/n
period' :: Integer -> Int
period' n
  | gcd n 10 /= 1 = period' (n `div` gcd n 10)
  | otherwise =
    let candidates = divisors (eulerPhi n)
     in head [fromInteger c | c <- candidates, modPow 10 c n == 1]

modPow :: Integer -> Integer -> Integer -> Integer
modPow b e 1 = 0
modPow b e m = modPow' b e m 1
  where
    modPow' b e 1 r = 0
    modPow' b 0 m r = r
    modPow' b e m r
      | e `mod` 2 == 1 = modPow' b' e' m (r * b `mod` m)
      | otherwise = modPow' b' e' m r
      where
        b' = b * b `mod` m; e' = e `div` 2