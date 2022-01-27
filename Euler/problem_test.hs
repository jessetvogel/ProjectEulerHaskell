import Data.Function (fix)

sideEffectFunction :: IO Int
sideEffectFunction = do
  print "sideEffectFunction!"
  return 3

main :: IO ()
main = do
  let s = sideEffectFunction

  s >>= print
  s >>= print

  print "The End!"
