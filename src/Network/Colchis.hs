{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Colchis (
        -- * Client 
        JSONClient (..)
   ,    JSONClientError (..)
   ,    call
        -- * Protocol 
   ,    Protocol
        -- * Transport
   ,    Transport
        -- * Running clients
   ,    runJSONClient
        -- * Utils
   ,    umap
   ,    umapM
        -- * Re-exported
   ,    hoist
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

import Network.Colchis.Protocol
import Network.Colchis.Transport

type JSONClientError = (Value,Text,Value)

type JSONClient s m r = Client (s,Value) Value (ExceptT JSONClientError m) r  

call :: (ToJSON a, FromJSON r, Monad m) => s -> a -> JSONClient s m r  
call s a = do
    let jreq = toJSON a
    rj <- request (s,jreq)
    case fromJSON rj of
        Error msg -> lift $ throwE (jreq,pack msg,rj)     
        Success r -> return r     

umap :: Monad m => (b' -> a') -> b' -> Proxy a' x b' x m r
umap f = go
  where
    go b = request (f b) >>= respond >>= go

umapM :: Monad m => (b' -> m a') -> b' -> Proxy a' x b' x m r
umapM f = go
  where
    go b = lift (f b) >>= request >>= respond >>= go

runJSONClient :: (MonadTrans t, MFunctor t, MonadIO m, Monad (t m)) => Transport t m -> Protocol s m e -> JSONClient s m r -> t m (Either e (Either JSONClientError r)) 
runJSONClient server adapter client = 
    runExceptT $ 
    runExceptT $
    runEffect $
    hoist (lift.lift) . server +>> hoist (lift.hoist lift) . adapter +>> hoist (hoist (lift.lift)) client
