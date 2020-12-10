import Data.List
import Data.List.Split

main = do
  contents <- getContents
  putStrLn $ show $ product $ map paths $ filter (not . null) $ splitWhen (==3) $ diffs $ prepareAdapters $ map readInt $ lines contents

paths :: [Int] -> Int
paths [] = 1
paths (1:[]) = 1
paths (2:[]) = 1
paths (1:1:xs) = (paths $ 1:xs) + (paths $ 2:xs)
paths (1:2:xs) = (paths $ 2:xs) + (paths xs)
paths (2:1:xs) = (paths $ 1:xs) + (paths xs)
paths (2:2:xs) = paths $ 2:xs

prepareAdapters :: [Int] -> [Int]
prepareAdapters xs = sort $ 0:(3 + maximum xs):xs

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (tail xs) (init xs)

readInt :: String -> Int
readInt = read
