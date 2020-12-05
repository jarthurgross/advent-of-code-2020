import Data.List.Split

main = do
  contents <- getContents
  putStrLn $ show $ product $ map (flip countTrees (lines contents)) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

countTrees :: (Int, Int) -> [String] -> Int
countTrees (r, d) = snd . (foldr (checkRow r) (0, 0)) . (stride d) . reverse

checkRow :: Int -> String -> (Int, Int) -> (Int, Int)
checkRow r s (idx, n) = (idx + 1, n + incr)
  where incr = if (concat $ repeat s) !! (r * idx) == '#' then 1 else 0

--debugging fn
getObjects :: (Int, Int) -> [String] -> String
getObjects (r, d) = reverse . snd . (foldr (getObject r) (0, "")) . (stride d) . reverse

--debugging fn
getObject :: Int -> String -> (Int, String) -> (Int, String)
getObject r s (idx, objs) = (idx + 1, ((concat $ repeat s) !! (r * idx)):objs)

stride :: Int -> [a] -> [a]
stride n = (map head) . (chunksOf n)
