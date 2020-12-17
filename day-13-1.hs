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

takeDigits :: String -> (String, String)
takeDigits "" = ("", "")
takeDigits (c:cs) = if c `elem` ['0' .. '9'] then (c:ds, ls) else ("", c:cs)
  where (ds, ls) = takeDigits cs

count :: Eq a => a -> [a] -> Int
count c = foldr (\c' n -> if c == c' then n + 1 else n) 0

mapCount :: Eq a => a -> Map k a -> Int
mapCount c = Map.foldr (\c' n -> if c == c' then n + 1 else n) 0

stride :: Int -> [a] -> [a]
stride n = (map head) . (chunksOf n)

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False
