module Network.Colchis.Adapter.JSONRPC20 (
       module Network.Colchis.Adapter
    ) where

import Network.Colchis.Adapter
import qualified Network.Colchis.Adapter.JSONRPC20.Request as IN
import qualified Network.Colchis.Adapter.JSONRPC20.Response as OUT 

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

type JSONRPC20Error = ()

-- http://www.jsonrpc.org/specification
adaptToJSONRPC20 :: Monad m => Adapter Text JSONRPC20Error m
adaptToJSONRPC20 = evalStateP 0 `liftM` go 
  where
    go (method,j) = do 
        msgId <- freshId                                
        undefined 
    freshId = lift $ withStateT (flip mod 100 . succ) get

