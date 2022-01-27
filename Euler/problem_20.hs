-- Factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Take the largest prime factor of 600851475143
main :: IO ()
main = print $ sum [(read :: String -> Integer) [c] | c <- show (factorial 100)]
