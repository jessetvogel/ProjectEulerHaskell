leadingDigits :: [Integer]
leadingDigits = map floor doubles where doubles = 1 : [if m' > 1000 then m' / 10 else m' | m <- doubles, let m' = 2 * m]

ns :: Integer -> [Integer]
ns l = [n | (n, m) <- zip [0 ..] leadingDigits, m == l]

main :: IO ()
main = print $ last $ take 678910 (ns 123)
