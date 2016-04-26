module Third where

-- Problem 21
insertAt :: a -> Int -> [a] -> [a]
insertAt x n xs = let (f,s) = splitAt n xs in f ++ x:s

-- Problem 22
range :: Int -> Int -> [Int]
range m n = take (n - m + 1) $ iterate (+1) m