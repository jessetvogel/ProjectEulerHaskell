import Data.Foldable (for_)
import Data.List (group, nub, sort)
import Euler.Util (isSquare)

-- Number of points that are STRICTLY contained in triangle ((0, 0), (x, 0), (0, y))
ptsInTriangle :: Integer -> Integer -> Integer
-- ptsInTriangle x y = sum [bMax | a <- [1 .. x - 1], let bMax = ceiling (fromInteger (y * (x - a)) / fromInteger x) - 1]
ptsInTriangle x y = 1 + (x * y - x - y - gcd x y) `div` 2 -- Quicker using Pick's theorem

-- Number of points in quad
ptsInQuad :: Integer -> Integer -> Integer -> Integer -> Integer
ptsInQuad a b c d = ptsInTriangle a b + ptsInTriangle b c + ptsInTriangle c d + ptsInTriangle d a + a + b + c + d - 3

uniquePermutations :: (Integer, Integer, Integer, Integer) -> [(Integer, Integer, Integer, Integer)]
uniquePermutations (a, b, c, d) = nub [(a, b, c, d), (b, c, d, a), (c, d, a, b), (d, a, b, c), (c, b, a, d), (b, a, d, c), (a, d, c, b), (d, c, b, a)]

m :: Integer
m = 100

main :: IO ()
main = do
  let goodQuads = [(a, b, c, d) | a <- [1 .. m], b <- [a .. m], c <- [a .. m], d <- [b .. m], let p = ptsInQuad a b c d, isSquare p]
  for_ goodQuads print
  let allQuads = concatMap uniquePermutations goodQuads
  print $ length $ group $ sort allQuads
