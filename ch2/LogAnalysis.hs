module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
                    ("I":t:m) -> LogMessage Info (read t) (unwords m)
                    ("W":t:m) -> LogMessage Warning (read t) (unwords m)
                    ("E":i:t:m) -> LogMessage (Error (read i)) (read t) (unwords m)
                    x -> Unknown $ unwords x

parse :: String -> [LogMessage]
parse n = map parseMessage (lines n)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert x Leaf = Node Leaf x Leaf 
insert lm@(LogMessage _ t _) (Node lt m@(LogMessage _ tm _) gt)
  | t < tm = Node (insert lm lt) m gt 
  | otherwise = Node lt m (insert lm gt)

build :: [LogMessage] -> MessageTree
build list = foldr (insert) Leaf list

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf m Leaf) = [m]
inOrder (Node fst m snd) = (inOrder fst) ++ [m] ++ (inOrder snd)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = map pullString $ filter wrongFilter ((inOrder . build) x)

wrongFilter :: LogMessage -> Bool
wrongFilter (LogMessage mt _ _) = case mt of
                                    Info    -> False
                                    Warning -> False
                                    (Error x) -> (x >= 50)

pullString :: LogMessage -> String
pullString (LogMessage _ _ s) = s
pullString _ = ""
