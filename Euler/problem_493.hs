import Euler.Util (choose)

-- Have n bins, each of k balls, and lets pick m balls
distributions :: Int -> Int -> Int -> [[Integer]]
distributions 1 k m = [[toInteger m] | m <= k]
distributions n k m = [toInteger p : d | p <- [0 .. min m k], d <- distributions (n - 1) k (m - p)]

weight :: Integer -> [Integer] -> Integer
weight k d = product [k `choose` x | x <- d]

main :: IO ()
main = do
  let ds = distributions 7 10 20
  let balls = map (length . filter (/= 0)) ds
  let ws = map (weight 10) ds
  let weightedBalls = sum [toInteger b * w | (b, w) <- zip balls ws]
  let totalWeight = sum ws

  print $ fromRational (toRational weightedBalls / toRational totalWeight)

-- What is the chance to get [a, b, c, d, e, f, g] ?

-- There are (10 nCr a) * (10 nCr b) * ... * (10 nCr g) possible ways to get the above outcome!
