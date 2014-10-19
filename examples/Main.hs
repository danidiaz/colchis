{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text

import Network.Colchis
import Network.Colchis.Protocol.JSONRPC20(jsonRPC20)
import Network.Colchis.Transport.TCP (runTcp,tcp)

import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Control.Concurrent(threadDelay)

exampleClient :: JSONClient Text IO Int
exampleClient = do
    i1 <- plusone 5 
    --liftIO $ threadDelay $ 10 * 10^6
    i2 <- plusone 7 
    return $ i1+i2
  where 
    plusone :: Int -> JSONClient Text IO Int
    plusone = call "plusone" 

-- When the base monad of the client is not IO,
-- it should be whittled to IO using "hoist" and
-- a suitable monad morphism. See main.
exampleClient2 :: JSONClient Text (ReaderT Int IO) Int
exampleClient2 = do
    i1 <- plusone 5 
    i2 <- plusone 7 
    i3 <- lift . lift $ ask
    return $ i1+i2+i3
  where 
    plusone :: Monad n => Int -> JSONClient Text n Int
    plusone = call "plusone" 

main :: IO ()
main = do
   r <- runTcp "localhost" "26060" $ 
            runJSONClient tcp jsonRPC20 exampleClient
   case r of
       Right (Right (Right i)) -> putStrLn $ "result: " ++ show i
       e -> putStrLn $ show e
   --
   r <- runTcp "localhost" "26060" $ 
            hoist (flip runReaderT 77) $
                runJSONClient tcp jsonRPC20 exampleClient2
   case r of
       Right (Right (Right i)) -> putStrLn $ "result: " ++ show i
       e -> putStrLn $ show e
