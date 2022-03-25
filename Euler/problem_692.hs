import Data.Foldable (for_)

{-

At any point, the game is described by two numbers (N = total number of pebbles, M = maximum number one can take)
Let W(N, M) = { 1 if winning game, 0 if losing game }
E.g.
    W(N, N) = 1
    W(2, 1) = 0, W(3, 1) = 0

Generally, W(N, M) = any ( W(N - k, 2 * k) == 0 for k <- [1 .. M] )
Let H(N) = min { k <- [1 .. N] | W(N - k, 2 * k) == 0 }

-}

w :: Integer -> Integer -> Bool
w 0 _ = False
w 1 _ = True
w n m = any (\k -> not $ w (n - k) (2 * k)) [1 .. m]

h :: Integer -> Integer
h n = head [k | k <- [1 .. n], not (w (n - k) (2 * k))]

{-

Let Fn be the Fibonacci sequence: F0 = 1, F1 = 1, F2 = 2, F3 = 3, F4 = 5, ...

Sequence H(N) =

    F1 : [ take (F0 - 1) H ] : F2 : [ take (F1 - 1) H ] : F3 : [ take (F2 - 1) H ] : F4 : [ take (F3 - 1) H ] : ...

-}

fib :: [Int]
fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

h' :: [Int]
h' = concat [b : take (a - 1) h' | (a, b) <- zip fib (tail fib)]

g :: [Int]
g = scanl1 (+) h'

g' :: [Integer]
g' = 1 : 3 : [(((n - 4) * n - 6) * a + ((n - 5) * n - 11) * b) `div` ((n - 6) * n - 1) | (n, a, b) <- zip3 [3 ..] g' (tail g')]

-- 23416728348467685 is the 80th Fibonacci number

main :: IO ()
main = print (g' !! 78)
