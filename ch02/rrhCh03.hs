import Data.List
-- Real World Haskell Ch03 Exercises

-- Write function that computes number of elements in a list
-- add a type signature
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Write a function that computes the mean of a list 
mean :: (Fractional a, Foldable t) => t a -> a
mean xs = (sum xs) / fromIntegral (length xs)

-- Turn a list into a palindrome
toPalindrome :: [a] -> [a]
toPalindrome xs = xs ++ reverse xs

-- Write a function that determines whether its input list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs
-- Below was my first go at it. After researching, the above is much more elegant
--isPalindrome [] = True
--isPalindrome (x:[]) = True
--isPalindrome (x:y:[]) = x == y
--isPalindrome xs = (head xs) == (last xs) && isPalindrome (init $ tail xs)

isAnagram :: String -> String -> Bool
isAnagram s1 s2 = (Data.List.sort s1) == (Data.List.sort s2)

-- Write a function that sorts a list of lists based on the length of each sublis
sortByLength :: [[a]] -> [[a]]
sortByLength xs = sortBy compareLength xs
    where compareLength a b = compare (length a) (length b)
    
-- Define a function that joins a list of lists together using a separator value
intersperse' :: a -> [[a]] -> [a]
intersperse' _ [] = []
intersperse' sep (x:[]) = x
intersperse' sep (x:xs) = x ++ sep : intersperse' sep xs

-- Creating our own list
data List a = Cons a (List a)
            | Nil
              deriving (Show)
              
fromList []     = Nil
fromList (x:xs) = Cons x (fromList xs)

fromList' Nil = []
fromList' (Cons x xs) = x : fromList' xs

-- Write a function to determine height of binary tree.
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
              
height :: Tree a -> Int
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)

-- Define a Direction data type representing Left, Right, Straight
data Direction = LeftTurn | Straight | RightTurn
    deriving (Show)

-- Write a function that calculates the turn made by three 2D points and returns a Direction.
data Point2D = Point2D Double Double
    deriving (Eq, Show)
    
direction :: Point2D -> Point2D -> Point2D -> Direction
direction (Point2D x1 y1) (Point2D x2 y2) (Point2D x3 y3) = 
    case compare ccw 0 of 
        GT -> LeftTurn
        EQ -> Straight
        LT -> RightTurn
    where ccw = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)
    
-- Define a function that takes a list of 2D points and computes the direction of each successive triple.
directions :: [Point2D] -> [Direction]
directions (p1:p2:p3:[]) = [direction p1 p2 p3]
directions (p1:p2:p3:ps) = direction p1 p2 p3 : directions (p2:p3:ps)
directions _ = error "Invalid list"

-- Implement Graham's scan algorithm for the convex hull of a set of 2D points
grahamScan :: [Point2D] -> [Point2D]
