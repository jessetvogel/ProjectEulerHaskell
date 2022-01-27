import Data.List (sort)

isPandigital :: String -> Bool
isPandigital s = sort s == "123456789"

main :: IO ()
main =
  print $
    maximum
      ( [s | n <- [1 .. 100000], let s = show n ++ show (2 * n), isPandigital s]
          ++ [s | n <- [1 .. 1000], let s = show n ++ show (2 * n) ++ show (3 * n), isPandigital s]
          ++ [s | n <- [1 .. 100], let s = show n ++ show (2 * n) ++ show (3 * n) ++ show (4 * n), isPandigital s]
          ++ [s | n <- [1 .. 10], let s = show n ++ show (2 * n) ++ show (3 * n) ++ show (4 * n) ++ show (5 * n), isPandigital s]
      )
