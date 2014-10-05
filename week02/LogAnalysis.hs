
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

timestamp :: LogMessage -> TimeStamp
timestamp (LogMessage _ ts _) = ts
timestamp _ = 0

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m (Node l m' r) =
  if timestamp m <= timestamp m'
  then Node (insert m l)  m' r
  else Node l m' (insert m r)

build :: [LogMessage] -> MessageTree
build []     = Leaf
build [x]    = insert x Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = (inOrder l) ++ [m] ++ (inOrder r)

isErrorAndSevere :: MessageType -> Bool
isErrorAndSevere (Error severity) = severity >= 50
isErrorAndSevere _ = False


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = whatWentWrongHelper (inOrder (build ms))

whatWentWrongHelper :: [LogMessage] -> [String]
whatWentWrongHelper [] = []
whatWentWrongHelper ((LogMessage mt _ s):ms) =
  if isErrorAndSevere mt
  then s:(whatWentWrongHelper ms)
  else whatWentWrongHelper ms

-- lm1 = parseMessage "W 3654 e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled"
-- lm2 = parseMessage "I 5053 pci_id: con ing!"
-- lm3 = parseMessage "E 55 1034 'What a pity it wouldn't stay!' sighed the Lory, as soon as it was quite"
-- r1 = (build [lm1, lm2, lm3])
-- r2 = inOrder (build [lm1, lm2, lm3])
-- wrong = whatWentWrong r2

-- Running the program:
-- wrongs = testWhatWentWrong parse whatWentWrong "error.log"

