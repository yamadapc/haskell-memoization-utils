module Control.Memoization.Utils
    ( memoize
    , memoizeLru
    , memoizeTime
    , constMemoize
    , constMemoizeTime
    ) where

import           Control.Concurrent.MVar (newMVar, putMVar, readMVar, swapMVar,
                                          takeMVar)
import qualified Data.Cache.LRU.IO       as LRU
import qualified Data.Map.Strict         as Map
import           Data.Time               (diffUTCTime, getCurrentTime)
import           Data.Time.Units         (Millisecond, toMicroseconds)

-- |
-- Caches function return values so that when the same arguments are provided
-- to a function, it's not called
memoize :: Ord a => (a -> IO b) -> IO (a -> IO b)
memoize action = do
    mm <- newMVar Map.empty
    return $ \arg -> do
        m <- takeMVar mm
        case Map.lookup arg m of
            Just ret -> do
                putMVar mm m
                return ret
            Nothing -> do
                ret <- action arg
                putMVar mm (Map.insert arg ret m)
                return ret

-- |
-- Version of 'memoize' that uses an LRU cache rather than always keeping results
-- in memory
memoizeLru :: Ord a => (Maybe Integer) -> (a -> IO b) -> IO (a -> IO b)
memoizeLru msize action = do
    lru <- LRU.newAtomicLRU msize
    return $ \arg -> do
        mret <- LRU.lookup arg lru
        case mret of
            Just ret -> return ret
            Nothing -> do
                ret <- action arg
                LRU.insert arg ret lru
                return ret

-- |
-- Version of 'memoize' keeps results for a certain amount of time
memoizeTime :: Ord a => Millisecond -> (a -> IO b) -> IO (a -> IO b)
memoizeTime delay action = do
    let delayDiff :: Double
        delayDiff = (fromInteger (toMicroseconds delay)) / 1000000
    mm <- newMVar Map.empty
    return $ \arg -> do
        m <- takeMVar mm
        case Map.lookup arg m of
            Just (ret, lastcall) -> do
                now <- getCurrentTime
                let diff = fromRational $ toRational $ diffUTCTime now lastcall
                if diff >= delayDiff
                    then do
                        ret' <- action arg
                        putMVar mm (Map.insert arg (ret', now) m)
                        return ret'
                    else do
                        putMVar mm m
                        return ret
            Nothing -> do
                now <- getCurrentTime
                ret <- action arg
                putMVar mm (Map.insert arg (ret, now) m)
                return ret

-- |
-- Caches an IO actions result
constMemoize :: IO b -> IO (IO b)
constMemoize action = do
    retmvar <- newMVar Nothing
    return $ do
        mret <- readMVar retmvar
        case mret of
            Just ret -> return ret
            Nothing -> do
                ret <- action
                _ <- swapMVar retmvar (Just ret)
                return ret

-- |
-- Caches an IO actions result for some amount of time
constMemoizeTime :: Millisecond -> IO b -> IO (IO b)
constMemoizeTime delay action = do
    let delayDiff :: Double
        delayDiff = (fromInteger (toMicroseconds delay)) / 1000000
    retmvar <- newMVar Nothing
    return $ do
        mret <- readMVar retmvar
        case mret of
            Just (ret, lastcall) -> do
                now <- getCurrentTime
                let diff = fromRational $ toRational $ diffUTCTime now lastcall
                if diff >= delayDiff
                    then do
                        ret' <- action
                        _ <- swapMVar retmvar (Just (ret, now))
                        return ret'
                    else return ret
            Nothing -> do
                ret <- action
                lastcall <- getCurrentTime
                _ <- swapMVar retmvar (Just (ret, lastcall))
                return ret
