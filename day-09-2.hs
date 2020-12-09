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
  putStrLn $ show $ (\(b, xs) -> (minimum xs) + (maximum xs)) $ head $ filter fst $ map (\(x, acc, xs) -> (90433990 - x `elem` acc, x:(take (length acc) xs))) $ map (\(x, xs) -> (x, takeWhile (<= 90433990 - x) $ accumulate xs, xs)) $ getHeadsTails $ map readInt $ lines contents

getHeadsTails :: [a] -> [(a, [a])]
getHeadsTails [] = []
getHeadsTails (x:xs) = (x, xs):(getHeadsTails xs)

accumulate :: [Int] -> [Int]
accumulate [] = []
accumulate (x:xs) = scanl' (+) x xs

readInt :: String -> Int
readInt = read

takeDigits :: String -> (String, String)
takeDigits "" = ("", "")
takeDigits (c:cs) = if c `elem` ['0' .. '9'] then (c:ds, ls) else ("", c:cs)
  where (ds, ls) = takeDigits cs

count :: Eq a => a -> [a] -> Int
count c = foldr (\c' -> \n -> if c == c' then n + 1 else n) 0

stride :: Int -> [a] -> [a]
stride n = (map head) . (chunksOf n)

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False
