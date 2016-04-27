module Third where

import System.Random

-- Problem 21
insertAt :: a -> Int -> [a] -> [a]
insertAt x n xs = let (f,s) = splitAt n xs in f ++ x:s

-- Problem 22
range :: Int -> Int -> [Int]
range m n = take (n - m + 1) $ iterate (+1) m

-- Problem 23
removeAt :: Int -> [a] -> [a]
removeAt n xs = let (f,s) = splitAt n xs in
    case s of []     -> xs
              (_:rs) -> f ++ rs

rndSelect :: (RandomGen g) => g -> Int -> [a] -> [a]
rndSelect _ _ [] = []
rndSelect _ 0 _  = []
rndSelect g n xs = let (i,ng) = randomR (0, length xs - 1) g in 
    (xs !! i) : rndSelect ng (n-1) (removeAt i xs)

-- Problem 24
rndRange :: (RandomGen g) => g -> Int -> Int -> [Int]
rndRange _ 0 _ = []
rndRange g m n = let (x,ng) = randomR (1, n) g in
    x : rndRange ng (m-1) n

-- Problem 25
rndPerm :: (RandomGen g) => g -> [a] -> [a]
rndPerm g xs = rndSelect g (length xs) xs

-- Problem 26
combinations :: Int -> [a] -> [[a]]
combinations = undefined
