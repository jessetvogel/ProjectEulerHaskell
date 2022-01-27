import Euler.Util (eulerPhi)

main :: IO ()
main = do
  let quotients = [(fromInteger n / fromInteger (eulerPhi n), n) | n <- [2 .. 1000000]]
  let (q, n) = maximum quotients
  print n
