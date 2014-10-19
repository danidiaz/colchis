-- |
-- This module defines the client monad and the type signatures for
-- transports and prototocols.
--
-----------------------------------------------------------------------------

{-# LANGUAGE Rank2Types #-}

module Network.Colchis (
        -- * Client 
        JSONClient (..)
   ,    JSONClientError (..)
   ,    call
        -- * Protocol 
   ,    Protocol
        -- * Transport
   ,    Transport
        -- * Running clients
   ,    runJSONClient
        -- * Utils
        -- $utils
   ,    umap
   ,    umapM
        -- * Re-exported
        -- $reexports
   ,    hoist
   )  where

import Data.Text
import Data.Aeson
import Control.Monad
import Control.Monad.Trans.Except
import Pipes
import Pipes.Core
import Pipes.Lift

import Network.Colchis.Protocol
import Network.Colchis.Transport


{-|
    (request associated with the error, error message, response that caused the error)
-}
type JSONClientError = (Value,Text,Value)

{-|
    Emits requests consisting in `Value`s paired with some metadata. The metadata is usually the method name.

    Receives `Value` responses.
-}
type JSONClient s m r = Client (s,Value) Value (ExceptT JSONClientError m) r  

call :: (ToJSON a, FromJSON r, Monad m) => s -> a -> JSONClient s m r  
call s a = do
    let jreq = toJSON a
    rj <- request (s,jreq)
    case fromJSON rj of
        Error msg -> lift $ throwE (jreq,pack msg,rj)     
        Success r -> return r     


{- $utils
    These functions can be used to manipulate requests flowing upstream.
-}

{-|
 Apply a function to all requests flowing upstream in a bidirectional pipe. Returns a function that can be composed with `+>>` or `>+>`.
 -}
umap :: Monad m => (b' -> a') -> b' -> Proxy a' x b' x m r
umap f = go
  where
    go b = request (f b) >>= respond >>= go

{-|
Apply a monadic function to all requests flowing upstream in a bidirectional pipe. Returns a function that can be composed with `+>>` or `>+>`.
-}
umapM :: Monad m => (b' -> m a') -> b' -> Proxy a' x b' x m r
umapM f = go
  where
    go b = lift (f b) >>= request >>= respond >>= go

{-|
    The return value lives inside the monad associated to the transport layer. The run function that peels off that layer depends on the transport. See for example `Network.Colchis.Transport.TCP.runTcp` for the `Network.Colchis.Transport.TCP.tcp` transport.
-}
runJSONClient :: (MonadTrans t, MFunctor t, MonadIO m, Monad (t m)) => Transport t m -> Protocol s m e -> JSONClient s m r -> t m (Either e (Either JSONClientError r)) 
runJSONClient server adapter client = 
    runExceptT $ 
    runExceptT $
    runEffect $
        hoist (lift.lift) . server 
        +>> 
        hoist (lift.hoist lift) . adapter 
        +>> 
        hoist (hoist (lift.lift)) client

{- $reexports
  
 When the function that runs the transport layer requires the underlying monad to be whittled down to `IO`, `hoist` (along with a suitable monad morphism) can come in handy.

-} 
