-- Define list of Fibonacci numbers
fibonacci :: [Integer]
fibonacci = 1 : 2 : [ a + b | (a, b) <- zip fibonacci (tail fibonacci) ]

-- Returns the initial list of numbers <= n
listUntil :: Integer -> [Integer] -> [Integer]
listUntil n [] = []
listUntil n (h : t) = if h > n then [] else h : listUntil n t

-- Print all even numbers that are <= 4 million
main::IO()
main = print (sum (filter even (listUntil 4000000 fibonacci)))
