{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Colchis.Transport.TCP (
        module Network.Colchis.Transport
    --,   TcpState
    ,   tcp
    ,   runTcp
    ,   TransportError(..)
    ,   ParsingError(..)
    )  where

import Data.Bifunctor
import Data.Text
import Data.Aeson
import Data.Aeson.Encode
import Data.IORef
import Data.Typeable
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Control.Concurrent.MVar
import Control.Concurrent.Conceit
import Network.Socket (shutdown,ShutdownCmd(ShutdownBoth))
import Pipes
import Pipes.Attoparsec
import Pipes.Core
import Pipes.Lift
import Pipes.Internal (unsafeHoist)
import Pipes.ByteString as PB
import Pipes.Network.TCP
import qualified Pipes.Prelude as P
import Pipes.Aeson
import Pipes.Aeson.Unchecked

import Network.Colchis.Transport

type TcpState = ReaderT (MVar (Maybe Value),MVar Value,IORef ConnState)  

data TransportError =
          RequestParsingError ParsingError
        | UnexpectedData 
        | UnexpectedConnectionClose
        deriving (Typeable,Show)

data ConnState =
          Idle
        | RequestSent
        | Finished
        deriving (Show)

producerFromMVar :: MVar (Maybe a) -> Producer a IO () 
producerFromMVar reqMVar = go
  where 
    go = do
       mj <- liftIO $ takeMVar reqMVar 
       case mj of 
           Nothing -> return ()
           Just j -> do
               yield j
               go

consumerFromMVar :: MVar a -> Consumer a IO x 
consumerFromMVar respMVar = forever $ await >>= liftIO . putMVar respMVar 

tcp :: Transport TcpState m
tcp = go
  where
    go req = do
        (reqMVar,respMVar,connState) <- lift ask
        liftIO $ atomicWriteIORef connState RequestSent  
        liftIO $ putMVar reqMVar (Just req)
        resp <- liftIO (takeMVar respMVar) 
        liftIO $ atomicWriteIORef connState Idle
        respond resp >>= go 


runTcp :: HostName -> ServiceName -> TcpState IO r -> IO (Either TransportError r) 
runTcp host port transport = 
    withSocketsDo $ connect host port $ \(sock,sockaddr) -> do
        reqMVar <- newEmptyMVar
        respMVar <- newEmptyMVar
        connState <- newIORef Idle 
        runConceit $ 
            (Conceit $ fmap pure $ do
                flip runReaderT (reqMVar,respMVar,connState) transport
                <*
                atomicWriteIORef connState Finished
                <*
                putMVar reqMVar Nothing
                <* 
                liftIO ( shutdown sock ShutdownBoth )
            )
            <*
            (Conceit $ fmap pure $ runEffect $
                for (producerFromMVar reqMVar) 
                    (yield . Data.Aeson.Encode.encode)
                >->
                toSocketLazy sock
            )
            <*
            (Conceit $ runEffect $ runExceptP $ 
                (jsonProducerFromSocket sock <* isPrematureClose connState) 
                >->
                connStateCheckerPipe connState
                >->
                hoist lift (consumerFromMVar respMVar)
            )
  where
    jsonProducerFromSocket sock = 
        hoist (withExceptT mkParsingError) $ 
        exceptP $
        view Pipes.Aeson.Unchecked.decoded (fromSocket sock 4096)
    isPrematureClose ior = do
        connState <- liftIO $ readIORef ior 
        case connState of
            Finished -> return ()
            _ -> lift $ throwE UnexpectedConnectionClose
    connStateCheckerPipe ior = forever $ do
        resp <- await
        connState <- liftIO $ readIORef ior 
        case connState of
            RequestSent -> yield resp
            _ -> lift $ throwE UnexpectedData
    mkParsingError (de,_) = RequestParsingError $ case de of
        AttoparsecError pe -> pe 
        FromJSONError _ -> error "never happens"  
    view l = getConst . l Const
    runExceptP = runExceptT . distribute
    exceptP p = do
        x <- unsafeHoist lift p
        lift $ ExceptT (return x)

