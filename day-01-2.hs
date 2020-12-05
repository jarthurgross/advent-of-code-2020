import Data.List

main = do
  contents <- getContents
  putStrLn $ show $ (\(x, y, z) -> x*y*z) $ head $ concat $ triples 2020 $ sort $ map (\x -> read x :: Int) $ words contents

doubles :: Int -> [Int] -> [(Int, Int)]
doubles _ [] = []
doubles _ (x:[]) = []
doubles tgt xs = (x, y):(doubles tgt remainder)
  where
    x = head xs
    y = last xs
    remainder = if x + y < tgt then tail xs else init xs

triples :: Int -> [Int] -> [[(Int, Int, Int)]]
triples _ [] = []
triples _ (x:[]) = []
triples _ (x:y:[]) = []
triples tgt (x:xs) = [(x, y, z) | (y, z) <- doubles (tgt - x) xs, x + y + z == tgt]:(triples tgt xs)
