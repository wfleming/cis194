{-# OPTIONS_GHC -Wall #-}
module LogAnalysis (parseMessage, parse) where

import Log

parseMessage :: String -> LogMessage
parseMessage s = case (parsePieces (words s)) of
  Just (mt, when, msg) -> LogMessage mt when msg
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
parseWhen (mt, time : pieces') = Just (mt, timeInt, pieces')
  where
    timeInt = read time :: Int -- TODO: how to capture errors here?
parseWhen _ = Nothing

parseMsg :: (MessageType, Int, [String]) -> Maybe (MessageType, Int, String)
parseMsg (mt, when, msgPieces) = Just (mt, when, unwords msgPieces)
parseMsg _ = Nothing

parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)
