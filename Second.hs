module Second where

import First

-- Problem 11
data Grouping a = Single a | Multiple Int a
    deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [Grouping a]
encodeModified = map enc . pack
    where enc l@(x:_) = case length l of
            1 -> Single x
            _ -> Multiple (length l) x

-- Problem 12
decodeModified :: [Grouping a] -> [a]
decodeModified = foldr dec []
    where dec (Single a) z     = a:z
          dec (Multiple l a) z = replicate l a ++ z

-- Problem 13
encodeDirect :: Eq a => [a] -> [Grouping a]
encodeDirect [] = []
encodeDirect (x:xs) = let (f,s) = span (==x) xs in
    case length f of
        1 -> Single x : encodeDirect s
        _ -> Multiple (length f) x : encodeDirect s
        
-- Problem 14
dupli :: [a] -> [a]
dupli = foldr (\x a -> x:x:a) []

-- Problem 15
repli :: Int -> [a] -> [a]
repli n = foldr (\x a -> replicate n x ++ a) []

-- Problem 16
dropEvery :: Int -> [a] -> [a]
dropEvery n = d (n-1) where
    d _ []     = []
    d 0 (_:xs) = d (n-1) xs
    d s (x:xs) = x : d (s-1) xs
    
-- Problem 17
split :: Int -> [a] -> ([a], [a])
split n = sp n [] where
    sp 0 fs ss = (fs, ss)
    sp e fs (s:ss) = sp (e-1) (fs ++ [s]) ss
    
-- Problem 18
slice :: Int -> Int -> [a] -> [a]
slice m n = take (n - m + 1) . drop (m - 1)

-- Problem 19
rotate :: Int -> [a] -> [a]
rotate n xs = let l = length xs
                  (f,s) = split ((l + n) `mod` l) xs 
              in s ++ f
              
-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = let (f, (s:ss)) = split n xs in (s, f ++ ss)