{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Colchis (
        JSONClientError (..)
   ,    JSONClient (..)
   ,    call
   ,    Adapter
   ,    TransportServer
   ,    runJSONClient
   ,    umap
   ,    umapM
   )  where

import Data.Text
import Data.Aeson
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Pipes
import Pipes.Core
import Pipes.Lift
import qualified Pipes.Prelude as P
import Pipes.Aeson

import Network.Colchis.Adapter
import Network.Colchis.Transport

type JSONClientError = (Text,Value)

type JSONClient s m r = Client (s,Value) Value (ExceptT JSONClientError m) r  

call :: (ToJSON a, FromJSON r, Monad m) => s -> a -> JSONClient s m r  
call s a = do
    rj <- request (s,toJSON a)
    case fromJSON rj of
        Error msg -> lift $ throwE (pack msg,rj)     
        Success r -> return r     

umap :: Monad m => (b' -> a') -> b' -> Proxy a' x b' x m r
umap f = go
  where
    go b = request (f b) >>= respond >>= go

umapM :: Monad m => (b' -> m a') -> b' -> Proxy a' x b' x m r
umapM f = go
  where
    go b = lift (f b) >>= request >>= respond >>= go

runJSONClient :: (MonadTrans t, MFunctor t, MonadIO m, Monad (t m)) => Adapter s ea m -> TransportServer t m -> (forall a. m a -> IO a) -> (forall a. t IO a -> IO (Either et a)) -> JSONClient s m r -> IO (Either et (Either ea (Either JSONClientError r))) 
runJSONClient adapter server morphism runTransport client = 
    runTransport $
    hoist morphism $
    runExceptT $ 
    runExceptT $
    runEffect $
    foo 
  where
    --foo:: Effect (ExceptT JSONClientError (ExceptT ea (t m))) r
    foo = hoist (lift.lift) . server +>> hoist (lift.hoist lift) . adapter +>> hoist (hoist (lift.lift)) client
