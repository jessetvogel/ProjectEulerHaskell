import Euler.Util (chunksOf, fileGetContents, splitBy)

containsOrigin :: [[Float]] -> Bool
containsOrigin [[x1, y1], [x2, y2], [x3, y3]] =
  length
    ( filter
        (> pi)
        [ abs (atan2 x2 y2 - atan2 x1 y1),
          abs (atan2 x3 y3 - atan2 x2 y2),
          abs (atan2 x1 y1 - atan2 x3 y3)
        ]
    )
    == 1
containsOrigin t = False

main :: IO ()
main = do
  triangles <- map (chunksOf 2 . map (read :: String -> Float) . splitBy ',') . lines <$> fileGetContents "data/data_102.txt"
  print $ length $ filter containsOrigin triangles
