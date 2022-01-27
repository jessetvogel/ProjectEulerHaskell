import Euler.Util (isSquare, isqrt, listUntil)

-- Type 1 has sides (l, l, l + 1)
-- Type 2 has sides (l, l, l - 1)

-- isType1 :: Integer -> Bool
-- isType1 l = isSquare d && (l - 1) * isqrt d `mod` 4 == 0 where d = 4 * l ^ 2 - (l - 1) ^ 2

-- isType2 :: Integer -> Bool
-- isType2 l = isSquare d && (l + 1) * isqrt d `mod` 4 == 0 where d = 4 * l ^ 2 - (l + 1) ^ 2

main :: IO ()
main = do
  let list1 = listUntil (> 1000000000) $ map (\i -> 3 * f1 i + 1) [1 ..]
  let list2 = listUntil (> 1000000000) $ map (\i -> 3 * f2 i - 1) [1 ..]

  print $ sum list1 + sum list2
--   print list1
--   print list2

--   print $ map f1 [1..7]
--   print $ map f2 [1..7]

f1 :: Int -> Integer
f1 0 = 1
f1 1 = 5
f1 2 = 65
f1 n = 15 * f1 (n - 1) - 15 * f1 (n - 2) + f1 (n - 3)

f2 :: Int -> Integer
f2 0 = 1
f2 1 = 17
f2 2 = 241
f2 n = 15 * f2 (n - 1) - 15 * f2 (n - 2) + f2 (n - 3)
