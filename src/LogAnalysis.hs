{-# OPTIONS_GHC -Wall #-}
module LogAnalysis (
  parseMessage,
  parse,
  insert,
  build,
  inOrder
) where

import Log

parseMessage :: String -> LogMessage
parseMessage s = case parsePieces (words s) of
  Just (mtype, when, msg) -> LogMessage mtype when msg
  Nothing -> Unknown s

parsePieces :: [String] -> Maybe (MessageType, Int, String)
parsePieces pieces = parseType pieces >>= parseWhen >>= parseMsg

parseType :: [String] -> Maybe (MessageType, [String])
parseType ("E" : code : pieces') = Just (Error codeInt, pieces')
  where
    codeInt = read code :: Int -- TODO: how to capture errors here?
parseType ("W" : pieces') = Just (Warning, pieces')
parseType ("I" : pieces') = Just (Info, pieces')
parseType _ = Nothing

parseWhen :: (MessageType, [String]) -> Maybe (MessageType, Int, [String])
parseWhen (mtype, time : pieces') = Just (mtype, timeInt, pieces')
  where
    timeInt = read time :: Int -- TODO: how to capture errors here?
parseWhen _ = Nothing

parseMsg :: (MessageType, Int, [String]) -> Maybe (MessageType, Int, String)
parseMsg (mtype, when, msgPieces) = Just (mtype, when, unwords msgPieces)

parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logmsg Leaf = Node Leaf logmsg Leaf
insert logmsg (Node left val right) =
  case logmsgWhen < valWhen of
    True -> Node (insert logmsg left) val right
    False -> Node left val (insert logmsg right)
  where
    (LogMessage _ logmsgWhen _) = logmsg
    (LogMessage _ valWhen _) = val

build :: [LogMessage] -> MessageTree
build lms = foldr insert Leaf lms

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left val right) = (inOrder left) ++ [val] ++ (inOrder right)
