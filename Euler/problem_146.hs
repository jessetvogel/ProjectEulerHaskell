import Data.Foldable (for_)
import Euler.MillerRabin (isPrime)
import System.Random (RandomGen, newStdGen)

-- import Euler.Util (isPrime, primes)

good :: (RandomGen g) => g -> Integer -> Bool
good seed n =
  and
    [ (n `mod` 2) `notElem` [1],
      (n `mod` 3) `notElem` [0],
      (n `mod` 5) `notElem` [1, 2, 3, 4],
      (n `mod` 7) `notElem` [0, 1, 2, 5, 6],
      (n `mod` 11) `notElem` [2, 3, 8, 9],
      (n `mod` 13) `notElem` [0, 2, 5, 6, 7, 8, 11],
      (n `mod` 17) `notElem` [2, 4, 5, 12, 13, 15],
      (n `mod` 19) `notElem` [4, 5, 7, 12, 14, 15],
      (n `mod` 23) `notElem` [4, 19],
      (n `mod` 29) `notElem` [4, 7, 12, 14, 15, 17, 22, 25],
      (n `mod` 31) `notElem` [2, 7, 11, 20, 24, 29],
      (n `mod` 37) `notElem` [6, 11, 16, 17, 18, 19, 20, 21, 26, 31],
      (n `mod` 41) `notElem` [9, 14, 27, 32],
      (n `mod` 43) `notElem` [4, 6, 13, 30, 37, 39],
      (n `mod` 47) `notElem` [9, 38],
      (n `mod` 53) `notElem` [16, 23, 24, 26, 27, 29, 30, 37],
      (n `mod` 59) `notElem` [20, 39],
      (n `mod` 61) `notElem` [11, 20, 27, 28, 29, 32, 33, 34, 41, 50],
      (n `mod` 67) `notElem` [8, 11, 23, 24, 43, 44, 56, 59],
      (n `mod` 71) `notElem` [8, 22, 49, 63],
      (n `mod` 73) `notElem` [8, 17, 22, 27, 46, 51, 56, 65],
      (n `mod` 79) `notElem` [17, 25, 32, 47, 54, 62],
      (n `mod` 83) `notElem` [30, 53],
      (n `mod` 89) `notElem` [13, 34, 55, 76],
      (n `mod` 97) `notElem` [19, 22, 26, 31, 66, 71, 75, 78],
      (n `mod` 101) `notElem` [10, 30, 47, 54, 71, 91],
      (n `mod` 103) `notElem` [10, 30, 73, 93],
      (n `mod` 107) `notElem` [10, 97],
      (n `mod` 109) `notElem` [10, 18, 33, 50, 54, 55, 59, 76, 91, 99],
      (n `mod` 113) `notElem` [10, 15, 28, 45, 68, 85, 98, 103],
      (n `mod` 127) `notElem` [10, 39, 45, 82, 88, 117],
      (n `mod` 131) `notElem` [],
      (n `mod` 137) `notElem` [26, 33, 37, 100, 104, 111],
      (n `mod` 139) `notElem` [23, 54, 85, 116],
      (n `mod` 149) `notElem` [17, 44, 69, 80, 105, 132],
      (n `mod` 151) `notElem` [12, 17, 44, 65, 86, 107, 134, 139],
      (n `mod` 157) `notElem` [12, 25, 28, 73, 75, 82, 84, 129, 132, 145],
      (n `mod` 163) `notElem` [25, 46, 51, 65, 98, 112, 117, 138],
      (n `mod` 167) `notElem` [34, 133],
      (n `mod` 173) `notElem` [67, 80, 81, 92, 93, 106],
      (n `mod` 179) `notElem` [72, 107],
      (n `mod` 181) `notElem` [19, 57, 63, 71, 84, 97, 110, 118, 124, 162],
      (n `mod` 191) `notElem` [39, 152],
      (n `mod` 193) `notElem` [24, 46, 50, 72, 81, 112, 121, 143, 147, 169],
      (n `mod` 197) `notElem` [14, 42, 92, 105, 155, 183],
      (n `mod` 199) `notElem` [14, 42, 157, 185],
      (n `mod` 211) `notElem` [29, 41, 87, 124, 170, 182],
      (n `mod` 223) `notElem` [14, 79, 87, 136, 144, 209],
      (n `mod` 227) `notElem` [21, 206],
      (n `mod` 229) `notElem` [40, 92, 107, 109, 120, 122, 137, 189],
      isPrime seed (n ^ 2 + 1),
      isPrime seed (n ^ 2 + 3),
      not $ isPrime seed (n ^ 2 + 5),
      isPrime seed (n ^ 2 + 7),
      isPrime seed (n ^ 2 + 9),
      not $ isPrime seed (n ^ 2 + 11),
      isPrime seed (n ^ 2 + 13),
      not $ isPrime seed (n ^ 2 + 15),
      not $ isPrime seed (n ^ 2 + 17),
      not $ isPrime seed (n ^ 2 + 19),
      not $ isPrime seed (n ^ 2 + 21),
      not $ isPrime seed (n ^ 2 + 23),
      not $ isPrime seed (n ^ 2 + 25),
      isPrime seed (n ^ 2 + 27)
    ]

ns :: (RandomGen g) => g -> [Integer]
ns seed = 10 : filter (good seed) [2, 4 .. 150 * 10 ^ 6]

main :: IO ()
main = do
  seed <- newStdGen
  let ns' = ns seed
  for_ ns' print
  print $ sum ns'

-- main = do
--   for_
--     (take 50 primes)
--     ( \p -> do
--         let badmods = [k | k <- [0 .. p - 1], any ((== 0) . (\r -> (k ^ 2 + r) `mod` p)) [1, 3, 7, 9, 13, 27]]
--         putStrLn $ "(n `mod` " ++ show p ++ ") `notElem` " ++ show badmods ++ ","
--     )
