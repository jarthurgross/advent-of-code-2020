import Data.List
import Data.List.Split
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Math.Sequence.Converge

main = do
  contents <- getContents
  putStrLn $ show $ getProduct $ parseInput $ lines contents

parseInput :: [String] -> (Int, [Int])
parseInput [] = (0, [])
parseInput (x:[]) = (readInt x, [])
parseInput (x:y:_) = (readInt x, map readInt $ filter (/= "x") $ splitOn "," y)

minutesToNext :: Int -> Int -> Int
minutesToNext t f = (- (t `mod` f)) `mod` f

getWaitTimes :: (Int, [Int]) -> [Int]
getWaitTimes (t, fs) = map (minutesToNext t) fs

getProduct :: (Int, [Int]) -> Int
getProduct (t, fs) = (uncurry (*)) $ minimum $ zip (getWaitTimes (t, fs)) fs

readInt :: String -> Int
readInt = read
