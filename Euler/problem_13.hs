
import System.IO
import Control.Monad
import Data.Char

-- Read file contents
fileGetContents :: String -> IO String
fileGetContents path = do
  handle <- openFile path ReadMode
  hGetContents handle

-- Compute sum and print it
main :: IO ()
main = do
    str <- fileGetContents "data/data_13.txt"
    putStrLn $ take 10 $ show $ sum $ map (read :: String -> Integer) (lines str)
