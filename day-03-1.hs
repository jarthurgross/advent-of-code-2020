main = do
  contents <- getContents
  putStrLn $ show $ foldr checkRow (1, 0) $ reverse $ tail $ lines contents

checkRow :: String -> (Int, Int) -> (Int, Int)
checkRow s (idx, n) = (idx + 1, n + incr)
  where incr = if (concat $ repeat s) !! (3 * idx) == '#' then 1 else 0
