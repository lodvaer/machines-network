{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- |
-- Module      :  Network.Machine
-- Copyright   :  Lodvær 2015
-- License     :  BSD3
--
-- Maintainer  :  Lodvær <lodvaer@gmail.com>
-- Stability   :  provisional
-- Portability :  unknown
--
-- Network machines.
module Network.Machine
    ( -- * TCP
      tcpServer
    , tcpClient
      -- * UDP
    , udp
      -- * Re-exports
    , ResourceT, runResourceT
    ) where

import Control.Applicative
import Control.Exception (IOException)
import Control.Monad
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (for_)
import Data.Machine
import qualified Network.BSD as NB
import qualified Network.Socket as So
import qualified Network.Socket.ByteString as BSo

import Data.Machine.Duplex

-- $setup
-- >>> addr <- So.SockAddrInet 9001 <$> So.inet_addr "127.0.0.1"
-- >>> import Control.Concurrent
-- >>> :set -XOverloadedStrings

-- | Forking TCP server.
--
-- >>> void . forkIO . runResourceT $ tcpServer addr (const reflect)
tcpServer :: (MonadThrow m, MonadIO m, MonadBaseControl IO m)
          => So.SockAddr
          -> (So.SockAddr -> ClientT (ResourceT m) ByteString ByteString)
          -> m ()
tcpServer addr comp = runResourceT $ do
    (_, so) <- allocate (So.socket So.AF_INET So.Stream NB.defaultProtocol)
                        So.close
    liftIO $ do
        So.bind so addr
        So.listen so 5
    forever $ do
        (k, (so', addr')) <- allocate (So.accept so) (So.close . fst)
        void . resourceForkIO .  runT_ $
            construct (socket k so') >~> comp addr'

-- | TCP client machine.
--
-- Send an empty string to close our half of the connection.
--
-- An empty string yielded means the peer has closed its half side of the
-- connection.
--
-- >>> runResourceT . runT_ $ tcpClient addr >~> (construct $ send "test" >> listen >>= liftIO . print)
-- "test"
tcpClient :: (MonadResource m, MonadIO m)
          => So.SockAddr
          -> ServerT m ByteString ByteString
tcpClient sa = construct $ do
    (k, so) <- lift $
        allocate (So.socket So.AF_INET So.Stream NB.defaultProtocol)
                 So.close
    liftIO $ So.connect so sa
    socket k so

-- | UDP. No difference between server and client. Non-forking.
--
-- TODO: this is crap because a read will block forever, making it
-- impossible to use this to multiplex many "connections" onto one socket.
--
-- >>> void . forkIO . runResourceT . runT_ $ udp (Just addr) >~> reflect
-- >>> runResourceT . runT_ $ udp Nothing >~> (construct $ send ("test", addr) >> listen >>= liftIO . print . fst)
-- "test"
udp :: (MonadIO m, MonadResource m)
    => Maybe So.SockAddr -- ^ Bind to this address if provided.
    -> ServerT m (ByteString, So.SockAddr) (ByteString, So.SockAddr)
udp maddr = construct $ do
    (_, so) <- lift $
        allocate (So.socket So.AF_INET So.Datagram NB.defaultProtocol)
                 So.close
    for_ maddr $ \addr -> liftIO $ So.bind so addr
    go so
  where
    go so =  (weakRecv >>= liftIO . uncurry (BSo.sendAllTo so) >> go so)
         <|> (liftIO (BSo.recvFrom so 65507) >>= yield >> go so)

-- Read/write from/to connected streaming sockets.
socket :: MonadResource m
       => ReleaseKey
       -> So.Socket
       -> PlanT (Channel x x' ByteString) ByteString m ()
socket k so = duplex
  where
    duplex =
        recv' <|> send'
    recv' = weakRecv >>= \x -> if B.null x then do
            liftIO $ So.shutdown so So.ShutdownSend
            simplexRecv <|> close
        else do
            soSend x
            duplex
    send' = do
        y <- soRecv
        yield y
        if B.null y then
            simplexSend <|> close
        else
            duplex
    simplexSend =
        recv >>= soSend >> simplexSend
    simplexRecv = do
        y <- soRecv
        yield y
        if B.null y then close else simplexRecv
    soRecv =
        liftIO $ BSo.recv so 4096
    soSend x = do
        y <- liftIO $
            (True <$ BSo.sendAll so x) -- TODO: no print.
                `catch` (\(e :: IOException) -> False <$ print e)
        unless y close

    close = release k >> stop
