import Data.List.Split
import qualified Data.Set as Set

main = do
  contents <- getContents
  putStrLn $ show $ sum $ map length $ map (foldr Set.union $ Set.fromList "") $ map (map Set.fromList) $ splitOn [""] $ lines contents
