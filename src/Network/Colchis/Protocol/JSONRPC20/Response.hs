{-# LANGUAGE DeriveGeneric #-}

module Network.Colchis.Protocol.JSONRPC20.Response 
    (
       Response(..)
    ,  ErrorObject(..)
    ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import GHC.Generics

data ErrorObject = ErrorObject
        {  _code :: Int
        ,  _message :: Text
        ,  _data :: Maybe Value
        } deriving (Generic,Show)

data Response = Response 
        {  _jsonrpc :: Text
        ,  _result  :: Maybe Value
        ,  _error :: Maybe ErrorObject
        ,  _id :: Value
        } deriving (Generic,Show)

options = defaultOptions { fieldLabelModifier = Prelude.tail , omitNothingFields = True }

instance FromJSON Response where
    parseJSON = genericParseJSON options

instance FromJSON ErrorObject where
    parseJSON = genericParseJSON options
