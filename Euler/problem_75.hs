import Data.List (sort)
import Euler.Util (isqrt)

primitiveTriples :: [[Integer]]
primitiveTriples =
  [ [a, b, c]
    | m <- [1 ..],
      n <- [1 .. m - 1],
      even n || even m,
      gcd m n == 1,
      let a = m ^ 2 - n ^ 2,
      let b = 2 * m * n,
      let c = m ^ 2 + n ^ 2
  ]

listUntil :: Integer -> [Integer] -> [Integer]
listUntil n [] = []
listUntil n (h : t) = if h > n then [] else h : listUntil n t

uniqueElements :: Eq a => [a] -> [a]
uniqueElements [] = []
uniqueElements [x] = [x]
uniqueElements lst =
  if x1 == x0
    then uniqueElements (omit x0 lst) -- if x0 == x1, then x0 is not unique, so omit all copies of x0
    else x0 : uniqueElements (tail lst) -- if x0 != x1, then x0 is unique, and we continue with the tail
  where
    x0 = head lst
    x1 = lst !! 1
    omit x [] = []
    omit x l = if head l == x then omit x (tail l) else l

main :: IO ()
main = do
  let m = 1500000
  let primitives = filter (<= m) $ listUntil (2 * m) (map sum primitiveTriples) -- first compute the 'primitive perimeters' below m
  let allPerimeters = concatMap (\l -> map (l *) [1 .. m `div` l]) primitives -- take all their multiples, still below m
  let sortedPerimeters = sort allPerimeters -- sort the perimeters
  let uniquePerimeters = uniqueElements sortedPerimeters -- find the unique perimeters
  print (length uniquePerimeters) -- print the number of unique perimeters
