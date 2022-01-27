skip :: Integer -> [a] -> [a]
skip 0 list = list
skip n list = skip (n - 1) (tail list)

waysToMake :: [Integer] -> Integer -> Integer
waysToMake coins amount
  | amount == 0 = 1 -- there is one way to make a sum of 0
  | null coins = 0 -- if we don't have coins, we cannot form a non-zero amount
  | head coins > amount = waysToMake (tail coins) amount -- if we can't use the first coin, may as well throw it away
  | otherwise = waysToMake coins (amount - head coins) + waysToMake (tail coins) amount -- either use the first coin or don't

main :: IO ()
main = print $ waysToMake [1, 2, 5, 10, 20, 50, 100, 200] 200
