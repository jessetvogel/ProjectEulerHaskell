-- Triangle numbers
triangleNumber :: Integer -> Integer
triangleNumber 0 = 0
triangleNumber n = n + triangleNumber (n - 1)

-- Divisors (technically should omit sqrt(n) if n is a square..)
divisors :: Integer -> [Integer]
divisors n = concat [ [k, n `div` k] | k <- [1..(floor . sqrt . fromIntegral) n], n `mod` k == 0 ]

-- Print the thing that is asked for
main :: IO ()
main = print (head [ triangleNumber n | n <- [1..], length (divisors (triangleNumber n)) >= 500 ])
