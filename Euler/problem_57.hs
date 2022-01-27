simplify :: (Integer, Integer) -> (Integer, Integer)
simplify (a, b) = (a `div` d, b `div` d) where d = gcd a b

fractions :: [(Integer, Integer)]
fractions = (3, 2) : [(a + 2 * b, a + b) | (a, b) <- fractions]

main :: IO ()
main =
  print $
    length $
      filter
        ( \(a, b) -> length (show a) > length (show b)
        )
        (take 1000 fractions)