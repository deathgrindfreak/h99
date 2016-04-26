module Second where

import First

-- Problem 11
data Grouping a = Single a | Multiple Int a

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
