import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

main = do
  contents <- getContents
  putStrLn $ show $ getProduct $ foldl (flip (Map.adjust (+1))) (Map.fromList [(1, 0), (2, 0), (3, 0)]) $ diffs $ prepareAdapters $ map readInt $ lines contents

getProduct :: Map Int Int -> Int
getProduct m = (Map.findWithDefault 0 1 m) * (Map.findWithDefault 0 3 m)

prepareAdapters :: [Int] -> [Int]
prepareAdapters xs = sort $ 0:(3 + maximum xs):xs

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (tail xs) (init xs)

readInt :: String -> Int
readInt = read
