{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Colchis ()  where

import Data.Aeson
import Control.Monad
import Control.Monad.Trans.Except
import Pipes
import Pipes.Core
import qualified Pipes.Prelude as P
import Pipes.Aeson

type JSONClient s m a = Client (s,Value) Value (ExceptT Value m) a  

call :: (ToJSON a, FromJSON r) => s -> a -> JSONClient s m r  
call = undefined


