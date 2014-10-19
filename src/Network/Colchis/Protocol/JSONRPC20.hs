{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Colchis.Protocol.JSONRPC20 (
      module Network.Colchis.Protocol
    , IN.ErrorObject (..)
    , JSONRPC20Error (..)
    , jsonRPC20  
    ) where

import Network.Colchis.Protocol
import qualified Network.Colchis.Protocol.JSONRPC20.Request as OUT
import qualified Network.Colchis.Protocol.JSONRPC20.Response as IN 

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

data JSONRPC20Error = 
        MalformedResponse Text Value
       |ProtocolMismatch Text
       |ResponseIdMismatch Int Int
       |ErrorResponse IN.ErrorObject
       deriving (Show)

-- http://www.jsonrpc.org/specification
jsonRPC20 :: Monad m => Protocol Text m (Text,Value,JSONRPC20Error)
jsonRPC20 = evalStateP 0 `liftM` go 
  where
    go (method,mkStructured -> j) = do 
        msgId <- freshId                                
        jresp <- request . toJSON $ OUT.Request protocolVer method j msgId
        let throwE' x = lift . lift . throwE $ (method,j,x)
        case parseEither parseJSON jresp of
            Left str -> throwE' $ MalformedResponse (pack str) jresp
            Right (IN.Response p' rm' em' id') -> do
                if protocolVer /= p' 
                    then throwE' $ ProtocolMismatch p'
                    else case em' of 
                      Just err -> throwE' $ ErrorResponse err
                      Nothing -> case rm' of
                         Nothing -> throwE' $ 
                            MalformedResponse "missing fields" jresp
                         Just val -> case parseEither parseJSON id' of 
                            Left str -> throwE' $ 
                              MalformedResponse "strange id" jresp
                            Right i -> if msgId /= i
                              then throwE' $ ResponseIdMismatch msgId i
                              else respond val >>= go 
    freshId = lift $ withStateT (flip mod 100 . succ) get
    protocolVer = "2.0"
    mkStructured j = case j of
        o@Object {} -> o
        a@Array {} -> a        
        Null -> emptyArray
        x -> toJSON $ [x]


