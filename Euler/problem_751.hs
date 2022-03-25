generatorToSequence :: Rational -> [Integer]
generatorToSequence q = a : generatorToSequence (a' * (q - a' + 1)) where a = floor q :: Integer; a' = toRational a

sequenceToGenerator :: [Integer] -> Rational
sequenceToGenerator xs =
  let s = concatMap show xs
      k = length s
   in toRational (read s) / 10 ^ (k - 1)

converge :: Rational -> Int -> Rational
converge q 0 = q
converge q n = converge (sequenceToGenerator (take 24 (generatorToSequence q))) (n - 1)

toDecimal :: Rational -> Int -> String
toDecimal q n = show (floor q) ++ "." ++ helper (10 * (q - toRational (floor q))) n
  where
    helper q' 0 = ""
    helper q' n = show (floor q') ++ helper (10 * (q' - toRational (floor q'))) (n - 1)

main :: IO ()
main = putStrLn $ toDecimal (converge 2 24) 24
