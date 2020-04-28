import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async (async, cancel, wait)
import Control.Exception (catch, displayException, SomeException(..))
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit
import Control.Monad (unless)
import Debug.Trace

finish :: [IO () -> IO b] -- a list of actions, each takes a canceller IO monad
       -> Int             -- the number of actions that are allowed to complete
       -> IO [b]          -- output messages of all actions
finish actions count = do
  -- ? Initialize `TMVar`s
  currentAction <- atomically $ newEmptyTMVar
  counter <- atomically $ newEmptyTMVar
  
  -- ? run action asynchronously and put its actionId on currentAction
  -- runAction :: (IO () -> IO a, Integer) -> IO (Async a)
  let runAction (action, actionId) = async $ action $ doTransaction actionId
        where doTransaction aId = do
                traceM("about to putTMVar currentAction with actionId " ++ (show $ actionId))
                atomically $ putTMVar currentAction aId
                atomically $ takeTMVar counter
                return ()

  -- ? run (race) async actions in parallel associating each with an id
  -- threads :: [Async a]
  threads <- mapM runAction $ zip actions [1..]

  -- ? gather 'count' results of raced threads based on which completes first
  -- raceThreads :: [Integer] -> IO ()
  let raceThreads actionIds = 
        if length actionIds < count
        then do 
          traceIO("running action and count actionIds is " ++ (show $ length $ actionIds))
          -- ? put completed actionId on list and "increment" counter
          actionId <- do 
            aId <- atomically $ takeTMVar currentAction
            traceM("got actionId: " ++ (show $ aId))
            atomically $ putTMVar counter ()
          -- ? get the next completed action until 'count' reached
            return aId
          raceThreads (actionId:actionIds)
        else 
          trace("Canceling threads with good ids " ++ (show $ actionIds))
          mapM_ cancelThread $ zip threads [1..]
          -- ? when max calls complete cancel others not in list, ignoring their results
          -- cancelThread :: (Async a, Integer) -> IO ()
            where cancelThread (action, actionId) = unless (actionId `elem` actionIds) (cancel action)
  -- ? really just "gather" results of raced threads from line 24
  raceThreads []
  -- ? wait for all asynchronous actions to complete
  mapM wait threads

main :: IO ()
main = do
  let urls = [("http://www.yahoo.com/", "test1.txt"),
              ("http://www.google.com/", "test2.txt"),
              ("http://www.msn.com/", "test3.txt"),
              ("http://www.bing.com/", "test4.txt")]

  let actions = map request urls
  ms <- finish actions 2
  mapM_ putStrLn ms
  where
    request :: (String, String) -> IO () -> IO String
    request (url, f) canceller = do
      d <- simpleHttp url
      canceller
      L.writeFile f d
      return $ url ++ " done"
      `catch` handler
      where handler = \(SomeException e) -> return (url ++ ": " ++ displayException e)