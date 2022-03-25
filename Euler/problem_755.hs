fib :: [Integer]
fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

decompose :: Integer -> [Integer]
decompose 0 = []
decompose n = a : decompose (n - a) where a = last (takeWhile (<= n) fib)

f :: Integer -> Integer
f n = helper n (drop 1 $ takeWhile (<= n) fib)
  where
    helper 0 _ = 1
    helper n [] = 0
    helper n lst = helper n (tail lst) + (if h <= n then helper (n - h) (tail lst) else 0) where h = head lst

main :: IO ()
main = print $ map (\n -> sum $ map f [0 .. n]) [0..100]
