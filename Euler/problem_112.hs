isIncreasing :: Ord a => [a] -> Bool
isIncreasing [] = True
isIncreasing [x] = True
isIncreasing (x : y : lst) = x <= y && isIncreasing (y : lst)

isDecreasing :: Ord a => [a] -> Bool
isDecreasing [] = True
isDecreasing [x] = True
isDecreasing (x : y : lst) = x >= y && isDecreasing (y : lst)

isBouncy :: Ord a => [a] -> Bool
isBouncy lst = not (isIncreasing lst) && not (isDecreasing lst)

numbers :: [(Integer, Integer)]
numbers = (0, 0) : [(b', t + 1) | (b, t) <- numbers, let b' = if isBouncy (show (t + 1)) then b + 1 else b]

main :: IO ()
main = print $ [t | (b, t) <- numbers, b * 100 == 99 * t] !! 1
