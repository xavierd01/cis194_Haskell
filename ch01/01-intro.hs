--01 intro 

-- Compute the sum of the integers from 1 to n.
sumtorial :: Integer -> Integer 
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

hailstone :: Integer -> Integer
hailstone n 
    | n `mod` 2 == 0 = n `div` 2
    | otherwise = 3*n + 1
    
isEven :: Integer -> Bool
isEven n = (n `mod` 2) == 0

-- Generate a sequence of hailstone iterations from a starting number.
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- Compute the length of a list of Integers
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (_:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []          = [] -- Do nothing on an empty list.
sumEveryTwo (x:[])      = [x] -- Do nothging to lists with a single element.
sumEveryTwo (x:y:zs)    = (x + y) : sumEveryTwo zs

-- The number of hailstone steps needed to reach 1 from a starting number.
hailstoneLength :: Integer -> Integer
hailstoneLength n = intListLength (hailstoneSeq n) - 1