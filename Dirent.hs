{-# LANGUAGE ScopedTypeVariables #-}

module Dirent
    ( Dirent(..)
    , makeDirent
    ) where

import           Control.Applicative  ((<$>))
import           Control.Concurrent   (Chan, forkIO, newChan, readChan, writeChan)
import           Control.Monad        (liftM, void)
import           Control.Monad.Trans  (liftIO)
import           Control.Monad.Writer (WriterT, execWriterT, tell)
import           Data.Set             (Set)
import qualified Data.Set             as S
import           System.Directory     (getDirectoryContents)
import           System.Posix.Files   (getFileStatus, isDirectory)
import           System.FilePath      ((</>))

import           ThreadManager        (forkManaged, withManager_)

data Dirent a = Directory FilePath (Set (Dirent a))
              | File FilePath a
              deriving Show

-- Compare Dirents on their file path; the tag is inconsequential.
instance Eq (Dirent a) where
    (Directory p1 _) == (Directory p2 _) = p1 == p2
    (File      p1 _) == (File      p2 _) = p1 == p2
    _ == _ = False

-- Order by file path, alphabetical.
instance Ord (Dirent a) where
    (Directory p1 _) <= (Directory p2 _) = p1 <= p2
    (Directory p1 _) <= (File      p2 _) = p1 <= p2
    (File      p1 _) <= (Directory p2 _) = p1 <= p2
    (File      p1 _) <= (File      p2 _) = p1 <= p2

type Handler a = FilePath -> IO a

makeDirent :: Handler a -> FilePath -> IO (Dirent a)
makeDirent f path = do
    is_dir <- isDirectory `liftM` getFileStatus path
    if is_dir
        then processDirectory f path
        else processFile f path

processDirectory :: forall a. Handler a -> FilePath -> IO (Dirent a)
processDirectory f path = do
    chan <- newChan                                -- Create channel to recieve (Maybe (Dirent a))s on
    void $ forkIO $ processDirectory' chan         -- Send (Maybe (Dirent a))s and then a Nothing
    Directory path `liftM` execWriterT (loop chan) -- Write sent (Just (Dirent a))s to children of Directory
  where
    processDirectory' :: Chan (Maybe (Dirent a)) -> IO ()
    processDirectory' chan = do
        withManager_ $ \threadManager ->
            getDirectoryContents' path >>= mapM_ (forkManaged threadManager . processPath' chan f)
        writeChan chan Nothing -- notify parent that all Dirents have been created

    -- Read from input channel until Nothing; collect children dirents in a Writer
    loop :: Chan (Maybe (Dirent a)) -> WriterT (Set (Dirent a)) IO ()
    loop chan = do
        val <- liftIO $ readChan chan
        whenJust val $ \a -> tell (S.singleton a) >> loop chan

-- | Like getDirectoryContents, but prepend the directory name, and remove "." and "..".
getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' path = map (path </>) . filter (`notElem` [".",".."]) <$> getDirectoryContents path

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just a) f = f a

processFile :: Handler a -> FilePath -> IO (Dirent a)
processFile f path = File path <$> f path

processPath :: Handler a -> FilePath -> IO (Dirent a)
processPath f path = do
    is_dir <- isDirectory <$> getFileStatus path
    if is_dir then processDirectory f path else processFile f path

-- Like processPath, but return the dirent on a channel (always Just).
processPath' :: Chan (Maybe (Dirent a)) -> Handler a -> FilePath -> IO ()
processPath' chan f path = processPath f path >>= writeChan chan . Just
