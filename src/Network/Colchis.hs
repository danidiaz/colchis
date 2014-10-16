{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Colchis ()  where

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

type JSONClient s m r = Client (s,Value) Value (ExceptT (String,Value) m) r  

call :: (ToJSON a, FromJSON r, Monad m) => s -> a -> JSONClient s m r  
call s a = do
    rj <- request (s,toJSON a)
    case fromJSON rj of
        Error msg -> lift $ throwE (msg,rj)     
        Success r -> return r     

upstream :: Monad m => (b' -> a') -> b' -> Proxy a' x b' x m r
upstream f = go
  where
    go b = request (f b) >>= respond >>= go


type JSONRPC20Error = ()

type JSONRPC20Adapter s m = forall r. (s,Value) -> Proxy Value Value (s,Value) Value (ExceptT JSONRPC20Error m) r

-- http://www.jsonrpc.org/specification
adaptToJSONRPC20 :: Monad m => ((s,Value) -> (Text,Value)) -> JSONRPC20Adapter s m
adaptToJSONRPC20 f = evalStateP 0 `liftM` go 
  where
    go (f -> (method,j)) = do 
        msgId <- freshId                                
        _
    freshId = lift $ withStateT (flip mod 100 . succ) get



