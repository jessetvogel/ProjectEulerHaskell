import Data.Foldable (for_)
import Data.List (nub, tails)
import Data.Ratio (denominator, numerator)
import Euler.Util (isqrt, primes)

valuation :: Integer -> Rational -> Integer
valuation p 0 = error "Cannot take valuation of 0"
valuation p n = val (numerator n) - val (denominator n)
  where
    val m = if m `mod` p /= 0 then 0 else 1 + val (m `div` p)

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x : xs) = [x : sublist | sublist <- sublists xs] ++ sublists xs

minima :: (Ord v) => (a -> v) -> [a] -> [a]
minima val lst = do
  let vals = map val lst
  let m = minimum vals
  map fst $ filter (\(x, v) -> v == m) (zip lst vals)

intCombs :: Integer -> [Rational] -> [Rational]
intCombs p lst = nub $ concat $ filter (\lst' -> let s = sum lst' in s /= 0 && valuation p s == 0) (sublists lst)

sieve :: Integer -> [Rational] -> [Rational]
sieve p lst = do
  let mins = minima (valuation p) lst -- Look at all numbers with minimal valuation w.r.t. p
  let goods = intCombs p mins -- Look at all numbers that can actually make an integral sum w.r.t. p
  let bads = [n | n <- mins, n `notElem` goods]
  filter (`notElem` bads) lst

-- duplicate :: Int -> [a] -> [a]
-- duplicate 0 _ = []
-- duplicate n lst = lst ++ duplicate (n - 1) lst

main :: IO ()
main = do
  let fractions = [1 / n ^ 2 :: Rational | n <- [2 .. 80]]
  let ps = drop 1 $ takeWhile (<= 80) primes
  let sievedFractions = foldl (flip sieve) fractions ps
  --   print $ map (isqrt . denominator) sievedFractions
  let solutions = make (1 / 2) sievedFractions
  for_ solutions (print . map (isqrt . denominator))
  print $ length solutions

make :: Rational -> [Rational] -> [[Rational]]
make 0 _ = [[]]
make q rs
  | sum rs < q = [] -- If using all numbers does not get us to q, might as well give up now
  | foldl lcm 1 (map denominator rs) `mod` denominator q /= 0 = [] -- The denominator of q should divide the lcm of the denominators of all rs
  | otherwise = concat [map (r :) $ make (q - r) (tail rs') | rs' <- tails rs, not (null rs'), let r = head rs', r <= q]
