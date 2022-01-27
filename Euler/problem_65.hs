import Data.Ratio (numerator)

contFracE :: [Integer]
contFracE = 2 : concatMap (\k -> [1, 2 * k, 1]) [1 ..]

contFracToRational :: [Integer] -> Rational
contFracToRational lst = foldl (\e a -> fromInteger a + 1 / e) (fromInteger $ head rlst) (tail rlst) where rlst = reverse lst

main :: IO ()
main = do
  let approx = contFracToRational $ take 100 contFracE
  let num = numerator approx
  let sumDigits = sum $ map (\c -> read [c]) (show num)
  print sumDigits