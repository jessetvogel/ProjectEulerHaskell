import Euler.Util (eulerPhi)

main :: IO ()
main = print $ sum $ map eulerPhi [2..1000000]
