import Data.List (sort)
import Euler.Util (eulerPhi)

isPermutation :: Integer -> Integer -> Bool
isPermutation m n = sort (show m) == sort (show n)

main :: IO ()
main = do
  print $
    snd $
      minimum
        [ (fromInteger n / fromInteger (eulerPhi n), n)
          | n <- [2 .. 10000000],
            isPermutation n (eulerPhi n)
        ]
