{-# LANGUAGE LambdaCase, TupleSections #-}

module ThreadManager
    ( ThreadManager     -- Hide implementation
    , ThreadStatus(..)
    , FinishedThreadStatus
    , forkManaged
    , getStatus
    , newManager
    , waitAll
    , waitAll_
    , waitFor
    , withManager
    , withManager_
    ) where

import           Control.Concurrent  (MVar, ThreadId, forkIO, newEmptyMVar, newMVar, modifyMVar, putMVar, takeMVar, tryTakeMVar)
import           Control.Exception   (SomeException, try)
import           Control.Monad       (void)
import           Data.Map            (Map)
import qualified Data.Map            as M

data ThreadStatus a = Running
                    | Finished a
                    | Threw SomeException

type FinishedThreadStatus a = Either SomeException a

newtype ThreadManager a = Mgr (MVar (Map ThreadId (MVar (ThreadStatus a))))

newManager :: IO (ThreadManager a)
newManager = Mgr `fmap` newMVar M.empty

forkManaged :: ThreadManager a -> IO a -> IO ThreadId
forkManaged (Mgr mgr) action =
    modifyMVar mgr $ \m -> do
        state <- newEmptyMVar
        tid <- forkIO $ try action >>= putMVar state . either Threw Finished
        return (M.insert tid state m, tid)

getStatus :: ThreadManager a -> ThreadId -> IO (Maybe (ThreadStatus a))
getStatus (Mgr mgr) tid = do
    modifyMVar mgr $ \m -> do
        case M.lookup tid m of
            Nothing      -> return (m, Nothing)
            Just mstatus -> tryTakeMVar mstatus >>= \case
                Nothing     -> return (m, Just Running)
                Just status -> return (M.delete tid m, Just status)

waitFor :: ThreadManager a -> ThreadId -> IO (Maybe (ThreadStatus a))
waitFor (Mgr mgr) tid = do
    modifyMVar mgr $ \m -> do
        case M.lookup tid m of
            Nothing      -> return (m, Nothing)
            Just mstatus -> fmap ((M.delete tid m,) . Just) (takeMVar mstatus)

waitAll :: ThreadManager a -> IO [FinishedThreadStatus a]
waitAll (Mgr mgr) = fmap (map finishedThreadStatus) $ modifyMVar mgr elems >>= mapM takeMVar
  where
    elems :: Map ThreadId (MVar (ThreadStatus a)) 
          -> IO (Map ThreadId (MVar (ThreadStatus a)), [MVar (ThreadStatus a)])
    elems = return . (M.empty,) . M.elems

    finishedThreadStatus :: ThreadStatus a -> FinishedThreadStatus a
    finishedThreadStatus (Finished a) = Right a
    finishedThreadStatus (Threw e)    = Left e
    finishedThreadStatus Running      = undefined -- will never be reached

waitAll_ :: ThreadManager a -> IO ()
waitAll_ = void . waitAll

-- | Execute an action with a new ThreadManager, and wait on all the launched threads.
withManager :: (ThreadManager a -> IO ()) -> IO [FinishedThreadStatus a]
withManager f = do
    manager <- newManager
    f manager
    waitAll manager

withManager_ :: (ThreadManager a -> IO ()) -> IO ()
withManager_ = void . withManager
