
import Control.Concurrent
import Control.Monad
import Control.Monad.Random
import Data.Array.IO

type Account     = Integer                    -- an individual account
type AccountNum  = Int                        -- index for an account
type AccountList = IOArray AccountNum Account

deposit :: AccountList -> AccountNum -> Integer -> IO ()
deposit accts acct = withdraw accts acct . negate

withdraw :: AccountList -> AccountNum -> Integer -> IO ()
withdraw accts acct amt = do
  cur <- readArray accts acct
  writeArray accts acct (cur - amt)

transfer :: AccountList -> Integer -> AccountNum -> AccountNum -> IO ()
transfer accts amt from to = do
  withdraw accts from amt
  deposit accts to amt

numAccounts :: Int
numAccounts = 10

mkAccounts :: IO AccountList
mkAccounts = newArray (1, numAccounts) 0

sumAccounts :: AccountList -> IO Integer
sumAccounts accts = sum `liftM` getElems accts

-- | Randomly, continuously transfer one dollar from one account to another
twiddle :: AccountList -> IO ()
twiddle accts = do
  randomAccts <- evalRandIO $ getRandomRs (1, numAccounts)
  go randomAccts
  where
    go (from:to:rest) = do
      transfer accts 1 from to
      go rest
    go _ = fail "Infinite list is exhausted?"

-- | Spawn `n` threads, all `twiddle`ing accounts
spawnThreads :: Int -> AccountList -> IO ()
spawnThreads 0 _ = return ()
spawnThreads n accts = do
  forkIO $ twiddle accts
  spawnThreads (n-1) accts

-- | Spawn 5 twiddling threads, printing the sum of the accounts every .5 sec
main :: IO ()
main = do
  accts <- mkAccounts
  spawnThreads 5 accts
  loop accts
  where
    loop accts = do
      value <- sumAccounts accts
      putStrLn $ "Sum of accounts: " ++ show value
      threadDelay 500000   -- 500,000 μs
      loop accts
  
