import Data.List.Split

main = do
  contents <- getContents
  putStrLn $ show $ count True $ map hasRequiredFields $ map getFields $ map concat $ map (map words) $ splitOn [""] $ lines contents

getFields :: [String] -> [String]
getFields [] = []
getFields (x:xs) = (take 3 x):(getFields xs)

hasRequiredFields :: [String] -> Bool
hasRequiredFields ss = and $ map ((flip contains) ss) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains y (x:xs) = if y == x then True else contains y xs

count :: Eq a => a -> [a] -> Int
count c = foldr (\c' -> \n -> if c == c' then n + 1 else n) 0
