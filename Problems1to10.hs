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
elementAt [] _ = Nothing
elementAt (x:_) 0 = Just x
elementAt (x:xs) n = elementAt xs (n-1)

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse lst = rev lst [] where
    rev [] acc = acc
    rev (x:xs) acc = rev xs (x:acc)
    
-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = and $ map (\(x, y) -> x == y) $ zip xs (reverse xs)

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem e) = e
flatten (List l) = map flatten l
