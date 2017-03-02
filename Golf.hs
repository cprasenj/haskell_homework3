module Golf where
import Data.List
import Data.List.Split

findEveryNth :: [a] -> Int -> [a]
findEveryNth list n = map head $ (chunksOf n) $ drop (pred n) list

skips :: [a] -> [[a]]
skips list = map (findEveryNth list) [1..(length list)]

lookUpLocalMaxima :: [Integer] -> [Integer] -> [Integer]
lookUpLocalMaxima list result
  | (<) (length list) 3 = result
  | and $ [(>)] <*> [second] <*> [first, third] = lookUpLocalMaxima rest $ result ++ [second]
  | otherwise = lookUpLocalMaxima rest result
  where first = head list
        second = head . (drop 1) $ list
        third = head . (drop 2) $ list
        rest = tail list

localMaxima :: [Integer] -> [Integer]
localMaxima list = lookUpLocalMaxima list []
