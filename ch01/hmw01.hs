-- CIS 194 Homework 1

{-- Validating Credit Card Numbers 
    In this section, you will implement the validation algorithm for
    credit cards. It follows these steps:

    • Double the value of every second digit beginning from the right.
      That is, the last digit is unchanged; the second-to-last digit is doubled;
      the third-to-last digit is unchanged; and so on. For example,
      [1,3,8,6] becomes [2,3,16,6].

    • Add the digits of the doubled values and the undoubled digits
      from the original number. For example, [2,3,16,6] becomes
      2+3+1+6+6 = 18
      
    • Calculate the remainder when the sum is divided by 10. For the
      above example, the remainder would be 8.
      
      If the result equals 0, then the number is valid.
--}

-- Convert an integer into a list of all it's digits.
-- For example:
--  toDigits 123 -> [1,2,3]
toDigits    :: Integer -> [Integer]
toDigits n 
    | n <= 0 = []
    | otherwise = map (\x -> read [x] :: Integer) (show n)

-- Convert an integer into a reversed list of all it's digits
-- For example:
--  toDigits 123 -> [3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleInt :: Integer -> Integer
doubleInt n = 2*n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther (x:[])   = [x]
doubleEveryOther (x:y:zs) = x : (doubleInt y) : (doubleEveryOther zs)

-- Take a list of Integers and sum all their digits.
-- For example:
--  sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (map (\x -> sum (toDigits x)) xs)

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigitsRev n)) `mod` 10 == 0


{-- The Towers of Hanoi 
    The Towers of Hanoi is a classic puzzle with a solution
    that can be described recursively.
    
    The only rules are
    • you may only move one disk at a time, and
    • a larger disk may never be stacked on top of a smaller one.

    For example, as the first move all you can do is move the topmost,
    smallest disk onto a different peg, since only one disk may be moved
    at a time

--}

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi d f t s = (hanoi (d-1) f s t) ++ (f, t) : (hanoi (d-1) s t f)