import Control.Monad (unless)

finish :: [IO () -> IO b] -> Int -> IO [b]
finish actions n =
  do  recv <- newEmptyMVar
      send <- newEmptyMVar
      let runAction (f, id) = async $ f $ putMVar recv id >> takeMVar send
      threads <- mapM runAction $ zip actions [1..]

      let g ids = if length ids < n
        then do id <- takeMVar recv
          putMVar send ()
          g (id:ids)
        else mapM_ f $ zip threads [1..]
          where f (t, id) unless (id `elem` lst) $ cancel t
      g []
      mapM wait threads

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