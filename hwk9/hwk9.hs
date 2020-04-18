import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (catch, displayException, SomeException(..))
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit

finish :: [IO () -> IO b] -- a list of actions, each takes a canceller IO monad
       -> Int             -- the number of actions that are allowed to complete
       -> IO [b]          -- output messages of all actions
-- finish actions n =

main = do
  let urls = [("http://www.yahoo.com/", "test1.txt"),
              ("http://www.google.com/", "test2.txt"),
              ("http://www.msn.com/", "test3.txt"),
              ("http://www.bing.com/", "test4.txt")]]

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
    'catch' handler
    where handle = \(SomeException e) -> return (url ++ ": " ++ displayException e)