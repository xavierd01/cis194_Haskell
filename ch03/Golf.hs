module Golf where

{- Exercise 1 -}
-- Description
skips :: [a] -> [[a]]
skips xs = [each i xs | i <- [1..length xs]]

-- Description
each :: Int -> [a] -> [a]
each n xs = [x | (i,x) <- zip [1..] xs, i `mod` n == 0]


{- Exercise 2 -}
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs) = if x < y && y > z
                         then y : localMaxima (y:z:zs)
                         else localMaxima (y:z:zs)
localMaxima _ = []


{- Exercise 3 -}
--inc :: (Integer, Integer) -> (Integer, Integer)
--inc (x, count) = (x, count+1)

--dec :: (Integer, Integer) -> (Integer, Integer)
--dec (x, count) = (x, count-1)

--update :: ((Integer, Integer) -> (Integer, Integer)) -> [(Integer, Integer)] -> Integer -> [(Integer, Integer)]
--update f tups i = map check tups 
--    where check tup@(n,_)
--            | n == i = f tup 
--            | otherwise = tup

histogram :: [Integer] -> String
histogram xs = graph (hist xs) ++ "\n==========\n0123456789\n"
    where 
        graph hs 
            | any (\(_,c) -> c > 0) hs = graph (map dec hs) ++ graphLine hs
            | otherwise                = ""
            where 
                graphLine hs = '\n' : [prnt c | (_, c) <- hs]
                    where prnt c | c > 0     = '*'
                                 | otherwise = ' '
        hist = foldl (\acc i -> update acc i) [(i,0) | i <- [0..9]]
        update tups i = map check tups 
            where check tup@(n,_)
                | n == i    = inc tup 
                | otherwise = tup
        inc (x, count) = (x, count+1)
        dec (x, count) = (x, count-1)
        
        
--hist :: [Integer] -> [(Integer,Integer)]
--hist = foldl (\acc i -> update inc acc i) [(i,0) | i <- [0..9]]

--graph :: [(Integer,Integer)] -> String
--graph hs 
--    | any (\(_,c) -> c > 0) hs = graph (map dec hs) ++ graphLine hs
--    | otherwise = ""

--graphLine :: [(Integer, Integer)] -> String
--graphLine hs = '\n' : [prnt c | (_, c) <- hs]
--    where prnt c | c > 0     = '*'
--                 | otherwise = ' '