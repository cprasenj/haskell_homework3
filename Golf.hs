module Golf where
import Data.List
import Data.List.Split

findEveryNth :: [a] -> Int -> [a]
findEveryNth list n = map head $ (chunksOf n) $ drop (pred n) list

skips :: [a] -> [[a]]
skips list = map (findEveryNth list) [1..(length list)] 
