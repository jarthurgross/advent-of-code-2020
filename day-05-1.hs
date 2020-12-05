import Data.List.Split

main = do
  contents <- getContents
  putStrLn $ show $ maximum $ map (\(a, b) -> 8 * (getRow' a) + (getCol' b)) $ map (splitAt 7) $ lines contents

getRow' :: String -> Int
getRow' s = sum $ zipWith (*) (iterate (*2) 1) $ map (\c -> if c == 'F' then 0 else 1) $ reverse s

getCol' :: String -> Int
getCol' s = sum $ zipWith (*) (iterate (*2) 1) $ map (\c -> if c == 'L' then 0 else 1) $ reverse s

-- Original versions of functions

splitRowCol :: String -> (String, String)
splitRowCol s = (take 7 s, reverse $ take 3 $ reverse s)

getRow :: String -> Int
getRow s = sum $ map (\(a, b) -> a * b) $ zip (map (\n -> 2^n) [0 .. 6]) (map (\c -> if c == 'F' then 0 else 1) $ reverse s)

getCol :: String -> Int
getCol s = sum $ map (\(a, b) -> a * b) $ zip (map (\n -> 2^n) [0 .. 2]) (map (\c -> if c == 'L' then 0 else 1) $ reverse s)
