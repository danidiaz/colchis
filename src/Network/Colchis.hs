{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Colchis ()  where

import Data.Text
import Data.Aeson
import Control.Monad
import Control.Monad.Trans.Except
import Pipes
import Pipes.Core
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

type JSONRPC20Adapter s m = (s,Value) -> Proxy Value Value (s,Value) (ExceptT JSONRPC20Error m) Value

adaptToJSONRPC20 :: Monad m => ((s,Value) -> (Text,Value)) -> JSONRPC20Adapter s m
adaptToJSONRPC20 = undefined
