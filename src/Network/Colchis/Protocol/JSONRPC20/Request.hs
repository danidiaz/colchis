{-# LANGUAGE DeriveGeneric #-}

module Network.Colchis.Protocol.JSONRPC20.Request 
    (
        Request(..)
    ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import GHC.Generics

data Request = Request 
             {  _jsonrpc :: Text
             ,  _method  :: Text
             ,  _params  :: Value
             ,  _id :: Int
             } deriving (Generic)

instance ToJSON Request where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = Prelude.tail , omitNothingFields = True }



