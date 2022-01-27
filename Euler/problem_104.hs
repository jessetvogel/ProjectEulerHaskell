import Data.List (sort)

fibonacci :: [Integer]
fibonacci = 1 : 1 : [a + b | (a, b) <- zip fibonacci (tail fibonacci)]

isPandigital :: String -> Bool
isPandigital s = sort s == "123456789"

main :: IO ()
main =
  print $
    head
      [ n
        | (n, f) <- zip [1 ..] fibonacci,
          isPandigital (take 9 (show f)),
          isPandigital (show (f `mod` 1000000000))
      ]
