{-# LANGUAGE PackageImports #-}

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import "async" Control.Concurrent.Async (async, cancel, wait)
import Control.Exception (catch, displayException, SomeException(..))
import qualified Data.ByteString.Lazy as L
import "http-conduit" Network.HTTP.Conduit
import Control.Monad (unless)

finish :: [IO () -> IO b] -- a list of actions, each takes a canceller IO monad
       -> Int             -- the number of actions that are allowed to complete
       -> IO [b]          -- output messages of all actions
finish actions count = do
  -- ? Initialize `MVar`s
  currentAction <- newEmptyMVar
  counter <- newEmptyMVar
  
  -- runAction :: (IO () -> IO a, Integer) -> IO (Async a)
  let runAction (action, actionId) = async $ action $ putMVar currentAction actionId >> takeMVar counter

  threads <- mapM runAction $ zip actions [1..]

  -- raceThreads :: [Integer] -> IO ()
  let raceThreads actionIds = 
        if length actionIds < count
        then do 
          actionId <- takeMVar currentAction
          putMVar counter ()
          raceThreads (actionId:actionIds)
        else mapM_ cancelThread $ zip threads [1..]
          -- cancelThread :: (Async a, Integer) -> IO ()
          where cancelThread (action, actionId) = unless (actionId `elem` actionIds) (cancel action)
  raceThreads []
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