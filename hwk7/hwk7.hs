{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.State
import Control.Monad (forM_)
import System.Directory (doesDirectoryExist, listDirectory, Permissions(..), 
                         getModificationTime, getPermissions, getFileSize)
import System.FilePath ((</>), takeExtension)
import Control.Exception (handle, SomeException(..))

import Data.Time (UTCTime(..))
import Data.Time.Calendar(toGregorian)
import Data.Time.Clock(utctDay,UTCTime)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)

import Debug.Trace
import System.Directory

epochToUTC :: Integral a => a -> UTCTime
epochToUTC = posixSecondsToUTCTime . fromIntegral

epoch = epochToUTC 0

match :: String -> String -> Bool
match s p 
  | length s < l = False
  | otherwise = (take l s) == p || match (drop 1 s) p   
  where l = length p

class Applicative m => Predicate m where
   (>?) :: (Ord a) => m a -> a -> m Bool
   p >? a = (> a) <$> p 
   infixl 4 >?

   (<?) :: (Ord a) => m a -> a -> m Bool
   p <? a = (< a) <$> p 
   infixl 4 <?

   (==?) :: (Ord a) => m a -> a -> m Bool
   p ==? a = (== a) <$> p 
   infixl 4 ==?

   (!=?) :: (Ord a) => m a -> a -> m Bool
   p !=? a = (/= a) <$> p 
   infixl 4 !=?

   (~?) :: m String -> String -> m Bool
   p ~? s = (`match` s) <$> p 
   infixl 4 ~?

   (&&?) :: m Bool -> m Bool -> m Bool
   p1 &&? p2 = (&&) <$> p1 <*> p2 
   infixl 3 &&?

   (||?) :: m Bool -> m Bool -> m Bool
   p1 ||? p2 = (||) <$> p1 <*> p2 
   infixl 3 ||?

   notP :: m Bool -> m Bool
   notP = fmap (not) 

data FileInfo = FileInfo {
                    filePath :: FilePath,
                    filePermissions :: Maybe Permissions,
                    fileSize :: Maybe Integer,
                    fileLastModified :: Maybe UTCTime
                }  

newtype FilePredicate a = FileP { runFileP :: FileInfo -> a } deriving (Functor,Applicative)

instance Predicate FilePredicate 
 
pathP :: FilePredicate String
pathP = FileP filePath

extP :: FilePredicate String
extP = takeExtension <$> pathP

sizeP :: FilePredicate Integer
sizeP = maybe (-1) id <$> FileP fileSize 

getPermissionP :: (Permissions -> Bool) -> FilePredicate Bool
getPermissionP f = maybe False f <$> FileP filePermissions

searchP:: FilePredicate Bool
searchP = getPermissionP searchable 

readP :: FilePredicate Bool
readP = getPermissionP readable 

writeP :: FilePredicate Bool
writeP = getPermissionP writable 

execP :: FilePredicate Bool
execP = getPermissionP executable

timeP :: FilePredicate UTCTime
timeP = maybe epoch id <$> FileP fileLastModified
 
maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(SomeException _) -> return Nothing) (fmap Just act)

getInfo :: FilePath -> IO FileInfo
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (getFileSize path)
  modified <- maybeIO (getModificationTime path)
  return (FileInfo path perms size modified)


data FileAction = Done | Skip | Continue

type FileState s =  StateT s IO FileAction

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else 
        traceM $ show $ [path]
        return [path]
  return (concat paths)

find :: forall s. (FileInfo -> FileState s) -> FilePath -> FileState s
find getState path = do
  names <- lift $ listDirectory path -- ? lec 11 lift out of list monad (double list inside IO monad)
  
  iterate names 

      where iterate :: [FilePath] -> FileState s
        iterate [name] = do
          let path' = path </> name
          info <- lift $ getInfo path'
          s <- getState info
          return s

        iterate (name:names) = do
          let path' = path </> name
          -- traceM $ path'
          info <- lift $ getInfo path'
          s <- getState info
          let isDirectory = runFileP searchP info
          names' <- if isDirectory 
            then do 
            mNames <- lift $ listDirectory path'
            return mNames
            else return []
          let names'' = if isDirectory then names ++ names' else names
          -- traceM $ show $ names''
          case s of
            Done -> return s
            Continue -> iterate names''
            Skip -> iterate names''
            -- Continue -> iterate names''
            -- Skip -> iterate names''

-- ? getState should return a file state that decides how to fold file information, 
-- ? whether to skip a directory, and whether to stop
main = do
  -- let downloads = "C:\\Users\\tzhao\\Downloads"
  let downloads = "C:\\Users\\jwheaton\\Downloads"

  let yearP = ((\(x,_,_) -> x) . toGregorian . utctDay) <$> timeP 
  let recurseP = yearP >? 2018

  let filterP = (extP ==? ".pdf" &&? sizeP >? 2^22) ||? execP 

  let iter :: FileInfo -> FileState [(FilePath, Integer, UTCTime)]
      iter = \info -> do
            let eval = \p -> runFileP p info

            s <- get 
            let r = (eval pathP, eval sizeP, eval timeP) 
            let s' = if eval filterP then r : s else s
            put s'

            let a = if length s' >= 10 then Done 
                    else if eval recurseP then Continue else Skip
            return a
          
  results <- execStateT (find iter downloads) [] 
  forM_ results $ \(p,n,t) -> 
                            handle (\(SomeException _) -> return ()) 
                          $ putStrLn 
                          $ (take 100 p) ++ "\t" 
                              ++ show (round $ fromInteger n / 2^10) ++ " KB\t"
                              ++ show (utctDay t)