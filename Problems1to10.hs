module Problems1to10 where

-- Problem 1
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast [x] = error "singleton list"
myButLast (x:y:[]) = x 
myButLast (x:xs) = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> Maybe a
elementAt xs n = case drop n xs of
    [] -> Nothing
    (x:_) -> Just x

-- Problem 4
myLength :: [a] -> Int
myLength = foldr (\x a -> 1 + a) 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []
    
-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = and $ map (\(x, y) -> x == y) $ zip xs (reverse xs)

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten xs = undefined