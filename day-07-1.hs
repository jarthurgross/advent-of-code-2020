import Data.List.Split
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Math.Sequence.Converge

main = do
  contents <- getContents
  putStrLn $ show $ count True $ map (\x -> elem "shiny gold" $ snd x) $ Map.toList $ converge $ iterate mapAddBags $ Map.fromList $ map ((\(a, b) -> (intercalate " " $ take 2 a, parseContainedBags $ drop 1 b)) . (splitAt 3) . words) $ lines contents

mapAddBags :: Map String (Set String) -> Map String (Set String)
mapAddBags m = Map.mapWithKey (addBags m) m

addBags :: Map String (Set String) -> String -> Set String -> Set String
addBags m s ss =  Set.union ss $ Set.foldr (\b bs -> Set.union bs $ Map.findWithDefault Set.empty b m) Set.empty ss

parseContainedBags :: [String] -> Set String
parseContainedBags [] = Set.fromList []
parseContainedBags ("no":_) = Set.fromList []
parseContainedBags (_:x:y:z:bs) = Set.union (Set.singleton $ intercalate " " [x, y]) $ parseContainedBags bs

count :: Eq a => a -> [a] -> Int
count c = foldr (\c' -> \n -> if c == c' then n + 1 else n) 0
