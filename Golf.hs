module Golf where
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
localMaxima list = foldl (\x y -> let second = (!!) y 1 in if and $ [(>)] <*> [second] <*> [head y, last y] then x ++ [second] else x) [] $ createFrames list 3
