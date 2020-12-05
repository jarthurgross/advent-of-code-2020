import Data.List.Split

main = do
  contents <- getContents
  putStrLn $ show $ count True $ map isValidPassport $ map concat $ map (map words) $ splitOn [""] $ lines contents

getFields :: [String] -> [String]
getFields [] = []
getFields (x:xs) = (take 3 x):(getFields xs)

getFieldValuePairs :: [String] -> [(String, String)]
getFieldValuePairs [] = []
getFieldValuePairs (x:xs) = ((take 3 x), tail $ tail $ tail $ tail $ x):(getFieldValuePairs xs)

hasRequiredFields :: [String] -> Bool
hasRequiredFields ss = and $ map ((flip contains) ss) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validatePairs :: [(String, String)] -> Bool
validatePairs = and . (map validatePair)

isValidPassport :: [String] -> Bool
isValidPassport str = (hasRequiredFields $ getFields str) && (validatePairs $ getFieldValuePairs str)

validatePair :: (String, String) -> Bool
validatePair ("byr", val) = (1920 <= read val) && (2002 >= read val)
validatePair ("iyr", val) = (2010 <= read val) && (2020 >= read val)
validatePair ("eyr", val) = (2020 <= read val) && (2030 >= read val)
validatePair ("hgt", val) = if ls == "cm" then (150 <= read ds) && (193 >= read ds) else if ls == "in" then (59 <= read ds) && (76 >= read ds) else False
  where (ds, ls) = takeDigits val
validatePair ("hcl", val) = ('#' == head val) && (and $ map isHexDigit $ tail val)
validatePair ("ecl", val) = contains val ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validatePair ("pid", val) = (9 == length ds) && (0 == length ls)
  where (ds, ls) = takeDigits val
validatePair ("cid", _) = True
validatePair _ = False

isHexDigit :: Char -> Bool
isHexDigit c = contains c $ ['0' .. '9'] ++ ['a' .. 'f']

takeDigits :: String -> (String, String)
takeDigits ('1':cs) = ('1':ds, ls)
  where (ds, ls) = takeDigits cs
takeDigits ('2':cs) = ('2':ds, ls)
  where (ds, ls) = takeDigits cs
takeDigits ('3':cs) = ('3':ds, ls)
  where (ds, ls) = takeDigits cs
takeDigits ('4':cs) = ('4':ds, ls)
  where (ds, ls) = takeDigits cs
takeDigits ('5':cs) = ('5':ds, ls)
  where (ds, ls) = takeDigits cs
takeDigits ('6':cs) = ('6':ds, ls)
  where (ds, ls) = takeDigits cs
takeDigits ('7':cs) = ('7':ds, ls)
  where (ds, ls) = takeDigits cs
takeDigits ('8':cs) = ('8':ds, ls)
  where (ds, ls) = takeDigits cs
takeDigits ('9':cs) = ('9':ds, ls)
  where (ds, ls) = takeDigits cs
takeDigits ('0':cs) = ('0':ds, ls)
  where (ds, ls) = takeDigits cs
takeDigits cs = ("", cs)

contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains y (x:xs) = if y == x then True else contains y xs

count :: Eq a => a -> [a] -> Int
count c = foldr (\c' -> \n -> if c == c' then n + 1 else n) 0
