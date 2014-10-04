
{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-----------------------------
-- Exercise 1
-----------------------------

parseMessage :: String -> LogMessage
parseMessage s = if isValid (words s)
                 then parseMessageHelp (words s)
                 else Unknown s

parseMessageHelp :: [String] -> LogMessage
parseMessageHelp (a:b:msg) = if isError a
                             then LogMessage (Error (read b :: Int)) (read (head msg) :: Int) (unwords (tail msg))
                             else LogMessage (parseMessageType a) (read b :: Int) (unwords msg)
parseMessageHelp _ = Unknown "error"


-- A weak check for valid formats.
isValid :: [String] -> Bool
isValid (x:_) = x == "I" || x == "W" || x == "E"
isValid _ = False

isError :: String -> Bool
isError (x:_) = x == 'E'
isError _     = False

parseMessageType :: String -> MessageType
parseMessageType s = case head s of
                       'I' -> Info
                       'W' -> Warning
                       _   -> Error (-1)

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)


-----------------------------
-- Exercise 2
-----------------------------


