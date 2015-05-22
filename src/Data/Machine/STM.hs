{-# LANGUAGE RankNTypes #-}

-- |
-- Module      :  Data.Machine.STM
-- Copyright   :  Lodvær 2015
-- License     :  BSD3
--
-- Maintainer  :  Lodvær <lodvaer@gmail.com>
-- Stability   :  provisional
-- Portability :  unknown
--
-- Half duplexed machines.
module Data.Machine.STM where

import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Data.Machine
import Data.Void

sinkTMVar :: TMVar v -> ProcessT STM v Void
sinkTMVar tv = repeatedly $ await >>= lift . putTMVar tv

sourceTMVar :: TMVar v -> SourceT STM v
sourceTMVar tv = repeatedly $ lift (takeTMVar tv) >>= yield
