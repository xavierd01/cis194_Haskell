{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage [] = Unknown ""
parseMessage m = 
    let 
        ws = words m
        iTime = (read (head (tail ws)) :: Int)
        wTime = (read (head (tail ws)) :: Int)
        eSev  = (read (head (tail ws)) :: Int)
        eTime = (read (head (tail (tail ws))) :: Int)
        msg   = (unwords (tail (tail ws)))
        eMsg  = (unwords (tail (tail (tail ws))))
    in
        case head ws of
            "I" -> LogMessage Info iTime msg
            "W" -> LogMessage Warning wTime msg
            "E" -> LogMessage (Error eSev) eTime eMsg
            _   -> Unknown m


parse :: String -> [LogMessage]
parse ls = map parseMessage (lines ls)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lm Leaf = Node Leaf lm Leaf
insert lm node =
        if (time lm) <= (time (nodeMessage node))
        then Node (insert lm (left node)) (nodeMessage node) (right node)
        else Node (left node) (nodeMessage node) (insert lm (right node))
    where
        left (Node l _ _) = l
        right (Node _ _ r) = r
        nodeMessage (Node _ m _) = m
        time (LogMessage _ t _) = t
        
build :: [LogMessage] -> MessageTree
build msgs = foldl (\acc m -> insert m acc) Leaf msgs

-- | @testBuild b n f@ tests the MessageTree builder @b@ by running it
--   on the first @n@ lines of file @f@ using testParse. 
testBuild :: ([LogMessage] -> MessageTree) 
            -> Int 
            -> FilePath
            -> IO MessageTree
testBuild build n file = build <$> testParse parse n file

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l msg r) = (inOrder l) ++ msg : (inOrder r)

testInOrder :: (MessageTree -> [LogMessage])
            -> Int
            -> FilePath
            -> IO [LogMessage]
testInOrder inOrder n file = inOrder <$> testBuild build n file

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map (\(LogMessage _ _ m) -> m) (filter filterErrors (sort msgs))
                    where 
                        sort = (inOrder . build)
                        filterErrors (LogMessage (Error s) _ _) = s >= 50
                        filterErrors (LogMessage _ _ _) = False