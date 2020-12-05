import Data.List

main = do
  contents <- getContents
  putStrLn $ show $ head $ matching_products 2020 $ sort $ map (\x -> read x :: Int) $ words contents

target_sums :: Int -> [Int] -> [(Int, Int, Bool)]
target_sums _ [] = []
target_sums _ (x:[]) = []
target_sums tgt xs = (x, y, x + y == tgt):(target_sums tgt remainder)
  where
    x = head xs
    y = last xs
    remainder = if x + y < tgt then tail xs else init xs

matching_products :: Int -> [Int] -> [Int]
matching_products tgt xs = [x*y | (x, y, z) <- (target_sums tgt xs), z]
