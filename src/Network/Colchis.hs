{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Colchis (
        JSONClient (..)
   ,    call
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

type JSONClient s m r = Client (s,Value) Value (ExceptT (Text,Value) m) r  

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



