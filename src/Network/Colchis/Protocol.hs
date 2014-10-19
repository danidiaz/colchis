{-# LANGUAGE Rank2Types #-}

module Network.Colchis.Protocol (
        Protocol(..)        
    )  where

import Data.Aeson
import Control.Monad.Trans.Except
import Pipes


{-|
  A bidirectional `Proxy` waiting for a request, ready to be composed with `+>>` or `>+>`.

  `Protocol`s format incoming requests from downstream before sending them upstream. They also extract the values from returning protocol responses and send them downstream.

  `Protocol`s isolate clients from the specific details of each protocol. 
-}
type Protocol s m e = forall r. (s,Value) -> Proxy Value Value (s,Value) Value (ExceptT e m) r

