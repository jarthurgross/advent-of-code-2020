import Data.List.Split
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Math.Sequence.Converge

main = do
  contents <- getContents
  putStrLn $ show $ countBagsInGold $ Map.fromList $ map ((\(a, b) -> (intercalate " " $ take 2 a, parseContainedBags' $ drop 1 b)) . (splitAt 3) . words) $ lines contents

countBagsInGold :: Map String (Map String Int) -> Int
countBagsInGold mp = fst $ addBags'' mp $ last $ takeWhile (\(a, b) -> b /= Map.empty) $ iterate (addBags'' mp) (-1, Map.fromList [("shiny gold", 1)])

parseContainedBags' :: [String] -> Map String Int
parseContainedBags' [] = Map.empty
parseContainedBags' ("no":_) = Map.empty
parseContainedBags' (n:x:y:_:bs) = Map.insertWith (+) (intercalate " " [x, y]) (read n :: Int) $ parseContainedBags' bs

addBags'' :: Map String (Map String Int) -> (Int, Map String Int) -> (Int, Map String Int)
addBags'' mp (n, bs) = Map.foldrWithKey (\s m (m', bs') -> (m + m', Map.unionWith (+) (Map.map (*m) $ Map.findWithDefault Map.empty s mp) bs')) (n, Map.empty) bs

--Thought I could get away with a list of bag names at first, then realized
--I needed to keep track of how many of a particular kind had been added, since
--there may have been more than one parent bag.
addBags' :: Map String (Map String Int) -> (Int, [String]) -> (Int, [String])
addBags' m (n, ss) = Map.foldrWithKey (\s m (m', ss') -> (m + m', s:ss')) (n, []) $ Map.unionsWith (+) $ map (\s -> Map.findWithDefault Map.empty s m) ss
