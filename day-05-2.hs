import Data.List.Split
import Data.List

main = do
  contents <- getContents
  putStrLn $ show $ filter (not . snd) $ zip [1 .. 822] $ map ((flip elem) (sort $ map (\(a, b) -> 8 * (getRow a) + (getCol b)) $ map splitRowCol $ lines contents)) [1 .. 822]

splitRowCol :: String -> (String, String)
splitRowCol s = (take 7 s, drop 7 s)

getRow :: String -> Int
getRow s = sum $ map (\(a, b) -> a * b) $ zip (map (\n -> 2^n) [0 .. 6]) (map (\c -> if c == 'F' then 0 else 1) $ reverse s)

getCol :: String -> Int
getCol s = sum $ map (\(a, b) -> a * b) $ zip (map (\n -> 2^n) [0 .. 2]) (map (\c -> if c == 'L' then 0 else 1) $ reverse s)
