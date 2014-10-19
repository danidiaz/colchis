{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import Network.Colchis
import Network.Colchis.Adapter.JSONRPC20
import Network.Colchis.Transport.TCP

exampleClient :: JSONClient Text IO Int
exampleClient = do
    i1 <- plusone 5 
    i2 <- plusone 7 
    return $ i1+i2
  where 
    plusone :: Int -> JSONClient Text IO Int
    plusone = call "plusone" 


main :: IO ()
main = do
   r <- runTcpTransport "localhost" "26060" $ 
            runJSONClient tcpTransportServer jsonRPC20 exampleClient
   case r of
       Right (Right (Right i)) -> putStrLn $ "result: " ++ show i
       e -> putStrLn $ show e

