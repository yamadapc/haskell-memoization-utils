module Control.Memoization.UtilsSpec
  where

import           Control.Concurrent        (threadDelay)
import           Control.Memoization.Utils
import           Data.IORef
import           Data.Time.Units
import           Test.Hspec

spec :: Spec
spec = do
    describe "memoize" $ do
        it "calls our function on the first call" $ do
            ncalls <- newIORef (0 :: Int)
            let fn :: Int -> IO Int
                fn = \x -> do
                    modifyIORef ncalls (+ 1)
                    return $ x + 10
            fn' <- memoize fn
            ret1 <- fn' 10
            ret1 `shouldBe` 20
            ret2 <- fn' 10
            ret2 `shouldBe` 20
            calls <- readIORef ncalls
            calls `shouldBe` 1

    describe "constMemoizeTime" $ do
        it "calls our function only once every interval" $ do
            ncalls <- newIORef (0 :: Int)
            let fn :: IO Int
                fn = do
                    modifyIORef ncalls (+ 1)
                    return $ 20
            fn' <- constMemoizeTime (fromMicroseconds 200000) fn
            ret1 <- fn'
            ret1 `shouldBe` 20
            ret2 <- fn'
            ret2 `shouldBe` 20
            calls <- readIORef ncalls
            calls `shouldBe` 1
            threadDelay 200000
            ret3 <- fn'
            ret3 `shouldBe` 20
            calls' <- readIORef ncalls
            calls' `shouldBe` 2
