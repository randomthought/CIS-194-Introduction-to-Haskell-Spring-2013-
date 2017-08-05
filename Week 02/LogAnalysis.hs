{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage str = case words str of
    ("I":ts:msg)      -> LogMessage Info (read ts :: Int) (unwords msg)
    ("W":ts:msg)      -> LogMessage Warning (read ts :: Int) (unwords msg)
    ("E":code:ts:msg) -> LogMessage (Error (read code :: Int)) (read ts :: Int) (unwords msg)
    str               -> Unknown (unwords str)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown n) msgTree = msgTree
insert       logMsg (Leaf) = Node Leaf logMsg Leaf
insert msg@(LogMessage _ ts _) (Node left rootMsg@(LogMessage _ rootTs _) right) = case ts < rootTs of
    True  -> Node (insert msg left) rootMsg right
    False -> Node left rootMsg (insert msg right)

build :: [LogMessage] -> MessageTree
build         [] = Leaf
build (msg:msgs) = insert msg (build msgs)

inOrder :: MessageTree -> [LogMessage]
inOrder                     Leaf = []
inOrder (Node left logMsg right) = inOrder left ++ [logMsg] ++ inOrder right


