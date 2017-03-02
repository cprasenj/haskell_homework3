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
  | otherwise = if and [((>) second first), ((>) second third)]
                then lookUpLocalMaxima (tail list) (result ++ [second])
                else lookUpLocalMaxima (tail list) result
  where first = head list
        second = head . (drop 1) $ list
        third = head . (drop 2) $ list

localMaxima :: [Integer] -> [Integer]
localMaxima list = lookUpLocalMaxima list []
