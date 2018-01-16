-- file: ch02/lastButOne
lastButOne :: [a] -> a
lastButOne (x:y:[]) = x
lastButOne (x:[]) = error "*** Exception: lastButOne: single element list"
lastButOne [] = error "*** Exception: lastButOne: empty list"
lastButOne (_:xs) = lastButOne xs

