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
  putStrLn $ show $ fst $ foldl' sieve (0, 1) $ sortByModulus $ map getRemainder $ parseInput $ lines contents

parseInput :: [String] -> [(Int, Int)]
parseInput [] = []
parseInput (_:[]) = []
parseInput (_:x:_) = reverse $ snd $ foldr (\s (n, ss) -> (n + 1, if s /= "x" then (n, readInt s):ss else ss)) (0, []) $ reverse $ splitOn "," x

getRemainder :: (Int, Int) -> (Int, Int)
getRemainder (x, y) = ((-x) `mod` y, y)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

sortByModulus :: Ord a => Ord b => [(a, b)] -> [(a, b)]
sortByModulus = (map swap) . reverse . sort . (map swap)

sieve :: (Int, Int) -> (Int, Int) -> (Int, Int)
sieve (a1, n1) (a2, n2) = (head $ dropWhile (\x -> (x `mod` n2 /= a2)) [a1, a1 + n1 ..], n1 * n2)

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
