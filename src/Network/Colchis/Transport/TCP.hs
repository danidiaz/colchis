{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Colchis.Transport.TCP (
    )  where

import Data.Text
import Data.Aeson
import Data.Aeson.Encode
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Concurrent.Conceit
import Pipes
import Pipes.Attoparsec
import Pipes.Core
import Pipes.Lift
import Pipes.ByteString as PB
import Pipes.Network.TCP
import qualified Pipes.Prelude as P
import Pipes.Aeson.Unchecked

type TcpTransport m = ReaderT (MVar (Maybe Value),MVar Value) m 

type TcpTransportServer m = forall r. Value -> Server Value Value (TcpTransport m) r

mVarProducer :: MVar (Maybe a) -> Producer a IO () 
mVarProducer reqMVar = go
  where 
    go = do
       mj <- liftIO $ readMVar reqMVar 
       case mj of 
           Nothing -> return ()
           Just j -> do
               yield j
               go

mVarConsumer :: MVar a -> Consumer a IO x 
mVarConsumer respMVar = forever $ await >>= liftIO . putMVar respMVar 

runTcpTransport :: HostName -> ServiceName -> TcpTransport IO r -> IO (Either ParsingError r) 
runTcpTransport host port transport = 
    withSocketsDo $ connect host port $ \(sock,sockaddr) -> do
        reqMVar <- newEmptyMVar
        respMVar <- newEmptyMVar
        runConceit $ 
            (Conceit $ fmap pure $ do
                flip runReaderT (reqMVar,respMVar) transport
                <*
                putMVar reqMVar Nothing
            )
            <*
            (Conceit $ fmap pure $ runEffect $
                for (mVarProducer reqMVar) (yield . Data.Aeson.Encode.encode)
                >->
                toSocketLazy sock
            )
            <*
            (Conceit undefined)

