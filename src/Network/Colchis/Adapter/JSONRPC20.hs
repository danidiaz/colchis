{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Colchis.Adapter.JSONRPC20 (
       module Network.Colchis.Adapter
    ) where

import Network.Colchis.Adapter
import qualified Network.Colchis.Adapter.JSONRPC20.Request as OUT
import qualified Network.Colchis.Adapter.JSONRPC20.Response as IN 

import Data.Text
import Data.Aeson
import Data.Aeson.Types
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Pipes
import Pipes.Core
import Pipes.Lift
import qualified Pipes.Prelude as P
import Pipes.Aeson

type JSONRPC20Error = ()

-- http://www.jsonrpc.org/specification
adaptToJSONRPC20 :: Monad m => Adapter Text JSONRPC20Error m
adaptToJSONRPC20 = evalStateP 0 `liftM` go 
  where
    go (method,j) = do 
        msgId <- freshId                                
        let req = OUT.Request protocolVer
                              method
                              j
                              msgId
        jresp <- request $ toJSON req
        case parseEither parseJSON jresp of
            Left str -> throwE' ()
            Right (IN.Response p' rm' em' id') -> do
                if protocolVer /= p' 
                    then throwE' ()
                    else do
                        if msgId /= id'
                            then throwE' ()
                            else case em' of 
                                Just err -> throwE' ()
                                Nothing -> case rm' of
                                   Nothing -> throwE' ()
                                   Just val -> respond val >>= go 
    freshId = lift $ withStateT (flip mod 100 . succ) get
    protocolVer = "2.0"
    throwE' = lift . lift . throwE


