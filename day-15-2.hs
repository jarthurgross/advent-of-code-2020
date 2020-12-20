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
  nums <- return $ map readInt $ splitOn "," $ head $ lines contents
  putStrLn $ unlines $ map unwords $ map (map show) $ take 32 $ splitOn [0] $ unfoldr speakNumber $ initializeMemory nums

initializeMemory :: [Int] -> (Int, Int, Map Int Int)
initializeMemory nums = (1 + (length nums), last nums, Map.fromList $ zip nums [1, 2 ..])

speakNumber :: (Int, Int, Map Int Int) -> Maybe (Int, (Int, Int, Map Int Int))
speakNumber (turn, spoke, mem) = Just (s, (turn + 1, s, Map.insert spoke (turn - 1) mem))
  where t = Map.findWithDefault 0 spoke mem
        s = if t == 0 then 0 else turn - 1 - t

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
