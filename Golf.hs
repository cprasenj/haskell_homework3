module Golf where
import Data.List
import Data.List.Split

findEveryNth :: [a] -> Int -> [a]
findEveryNth list n = map head $ (chunksOf n) $ drop (pred n) list

skips :: [a] -> [[a]]
skips list = map (findEveryNth list) [1..(length list)]

createFrames :: [Integer] -> Integer -> [[Integer]]
createFrames list frameSize
  | (length list) < fromIntegral frameSize  = []
  | otherwise = take (fromIntegral frameSize) list : createFrames (tail list) frameSize

localMaxima :: [Integer] -> [Integer]
localMaxima list = foldl (\x y -> if and $ [(>)] <*> [(!!) y 1] <*> [head y, last y] then x ++ [(!!) y 1] else x) [] $ createFrames list 3
