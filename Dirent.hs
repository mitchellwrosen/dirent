{-# LANGUAGE ScopedTypeVariables #-}

module Dirent
    ( Dirent(..)
    , makeDirent
    ) where

import           Control.Applicative ((<$>))
import           Control.Exception   (SomeException)
import           Control.Monad       (liftM)
import           Data.Set            (Set)
import qualified Data.Set            as S
import           System.Directory    (getDirectoryContents)
import           System.Posix.Files  (getFileStatus, isDirectory)
import           System.FilePath     ((</>))

import           ThreadManager       (ThreadStatus(..), forkManaged, withManager)

-- A directory entry tagged with some data of type a. An exception could be
-- thrown trying to tag a dirent, hence the third data constructor.
data Dirent a = Directory       FilePath (Set (Dirent a))
              | File            FilePath a
              | ExceptionThrown SomeException
              deriving Show

-- Compare Dirents on their file path, the tag is inconsequential.
instance Eq (Dirent a) where
    (Directory p1 _)     == (Directory p2 _) = p1 == p2
    (File      p1 _)     == (File      p2 _) = p1 == p2
    _ == _ = False

instance Ord (Dirent a) where
    (Directory p1 _)     <= (Directory p2 _) = p1 <= p2
    (Directory p1 _)     <= (File      p2 _) = p1 <= p2
    (File      p1 _)     <= (Directory p2 _) = p1 <= p2
    (File      p1 _)     <= (File      p2 _) = p1 <= p2
    _ <= _ = False

-- User supplied handler does something with a FilePath in IO.
type Handler a = FilePath -> IO a

makeDirent :: Handler a -> FilePath -> IO (Dirent a)
makeDirent f path = do
    -- TODO catch exception
    is_dir <- isDirectory `liftM` getFileStatus path
    if is_dir
        then processDirectory f path
        else processFile f path

processDirectory :: forall a. Handler a -> FilePath -> IO (Dirent a)
processDirectory f path = Directory path . S.fromList . map threadStatusToDirent <$> processDirectory'
  where
    processDirectory' :: IO [ThreadStatus (Dirent a)]
    processDirectory' =
        withManager $ \threadManager -> do
            getDirectoryContents' path >>= mapM_ (forkManaged threadManager . processPath f)

    threadStatusToDirent :: ThreadStatus (Dirent a) -> Dirent a
    threadStatusToDirent (Finished d) = d
    threadStatusToDirent (Threw e)    = ExceptionThrown e
    -- Not reached because threads are waited on in withManager. Should probably refactor
    -- ThreadStatus for this reason.
    threadStatusToDirent Running = undefined

-- | Like getDirectoryContents, but prepend the directory name, and remove "." and "..".
getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' path = map (path </>) . filter (`notElem` [".",".."]) <$> getDirectoryContents path

processFile :: Handler a -> FilePath -> IO (Dirent a)
processFile f path = File path <$> f path

processPath :: Handler a -> FilePath -> IO (Dirent a)
processPath f path = do
    is_dir <- isDirectory <$> getFileStatus path
    if is_dir then processDirectory f path else processFile f path
