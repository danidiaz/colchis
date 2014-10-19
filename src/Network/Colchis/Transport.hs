{-# LANGUAGE Rank2Types #-}

module Network.Colchis.Transport (
        Transport(..)        
    )  where

import Data.Aeson
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Pipes.Core

{-|
  A pipes `Server` waiting for a request, ready to be composed with `+>>` or `>+>`.

  `Transport`s send requests over the wire and receive the responses.
-}
type Transport t m = (MonadIO m) => forall r. Value -> Server Value Value (t m) r
