-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE NoFieldSelectors      #-}

-- | A live connection to online-go.com, as frames in and frames out.
--
-- Three threads hold one of these up. One reads the socket and puts
-- what arrives on a stream. One drains everything waiting to be sent,
-- because a socket takes one writer and not three. The third does
-- nothing but ping, because this version of the protocol has the
-- client keeping the connection alive and the opening frame says how
-- often.
--
-- How to reach the server is an argument. Real life is a secure socket
-- to online-go.com, and a test points it at a server on this machine,
-- which is how everything below is checked without a network.
module Stones.Ogs.Socket
  ( Socket(..)
  , Dial
  , secure
  , open
  , openWith
  )
where

import           Control.Concurrent             ( threadDelay )
import qualified Control.Concurrent.Async      as Async
import           Control.Concurrent.MVar
import           Control.Concurrent.STM         ( atomically )
import           Control.Exception              ( SomeException
                                                , finally
                                                , try
                                                )
import           Control.Monad                  ( void )
import           Data.IORef
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Network.WebSockets            as WebSockets
import           Pipes                          ( Producer )
import           Pipes.Concurrent               ( Input
                                                , Output
                                                , fromInput
                                                , recv
                                                , send
                                                , spawn'
                                                , unbounded
                                                )
import qualified Wuss

import           Stones.Ogs.Frame

-- | A connection, once it is up.
data Socket = Socket
  { put   :: Packet -> IO (Either Text ())
    -- ^ Put one frame in the queue to be sent.
  , heard :: Producer (Either Text Packet) IO ()
    -- ^ Everything that arrives. A frame that cannot be read is a
    -- reason rather than a packet, and the last thing on the stream
    -- before it ends is why it ended.
  , stop  :: IO ()
    -- ^ Take it down.
  }

-- | How to reach a server and hand the connection to somebody.
type Dial = (WebSockets.Connection -> IO ()) -> IO ()

-- | The real one.
secure :: Dial
secure = Wuss.runSecureClient
  "online-go.com"
  443
  "/socket.io/?transport=websocket&EIO=3"

-- | Open a connection to online-go.com.
open :: IO (Either Text Socket)
open = openWith secure

-- | Open one to wherever this says.
openWith :: Dial -> IO (Either Text Socket)
openWith dial = do
  (toSend, sending, sealSending) <- spawn' unbounded
  (toHear, hearing, sealHearing) <- spawn' unbounded
  -- Filled once, with whether the connection came up. Everything
  -- after that is on the streams.
  ready  <- newEmptyMVar
  -- Whether it ever came up, which is a different question from
  -- whether the box above is full: the one waiting empties it by
  -- reading it.
  cameUp <- newIORef False
  worker <- Async.async
    (working dial toSend sending toHear ready cameUp
    `finally` atomically (sealSending >> sealHearing)
    )
  takeMVar ready >>= \case
    Left  why -> Left why <$ Async.cancel worker
    Right ()  -> pure
      (Right Socket
        { put   = \packet -> do
                    taken <- atomically (send toSend packet)
                    pure
                      (if taken
                        then Right ()
                        else Left "the connection has closed"
                      )
        , heard = fromInput hearing
        , stop  = Async.cancel worker
        }
      )

-- | The thread that owns the connection.
--
-- Whatever happens to it, somebody is told: the one that is waiting
-- for it to come up, or the stream if it came up and then went.
working
  :: Dial
  -> Output Packet
  -> Input Packet
  -> Output (Either Text Packet)
  -> MVar (Either Text ())
  -> IORef Bool
  -> IO ()
working dial toSend sending toHear ready cameUp = do
  outcome <- try (dial (session toSend sending toHear ready cameUp))
  let why = case outcome of
        Left (problem :: SomeException) ->
          "Lost the connection to online-go.com: " <> Text.pack (show problem)
        Right () -> "The connection to online-go.com closed."
  -- A connection that never came up is news for whoever is waiting for
  -- it. One that came up and then went is news for the stream, because
  -- by then nobody is waiting.
  up <- readIORef cameUp
  if up
    then void (atomically (send toHear (Left why)))
    else void (tryPutMVar ready (Left why))

-- | What happens on a connection that is up.
session
  :: Output Packet
  -> Input Packet
  -> Output (Either Text Packet)
  -> MVar (Either Text ())
  -> IORef Bool
  -> WebSockets.Connection
  -> IO ()
session toSend sending toHear ready cameUp connection = do
  first <- WebSockets.receiveData connection
  case decode first of
    Right (Open handshake) -> do
      writeIORef cameUp True
      putMVar ready (Right ())
      -- The reader is this thread, so the connection goes down when
      -- it does, and the other two go down with it.
      Async.withAsync (writing connection sending) $ \_ ->
        Async.withAsync (pinging toSend handshake.pingInterval)
          $ \_ -> reading connection toHear
    Right other ->
      putMVar ready (Left ("The socket opened with " <> encode other <> "."))
    Left why -> putMVar ready (Left ("The socket opened with " <> why <> "."))

-- | Read frames until the connection ends.
reading :: WebSockets.Connection -> Output (Either Text Packet) -> IO ()
reading connection toHear = do
  frame <- WebSockets.receiveData connection
  case decode frame of
    Right Close -> pure ()
    heard'      -> do
      taken <- atomically (send toHear heard')
      if taken then reading connection toHear else pure ()

-- | Send what is waiting, one at a time, because a socket takes one
-- writer.
writing :: WebSockets.Connection -> Input Packet -> IO ()
writing connection sending = atomically (recv sending) >>= \case
  Nothing     -> pure ()
  Just packet -> do
    WebSockets.sendTextData connection (encode packet)
    writing connection sending

-- | Ping for as long as anybody is listening.
--
-- The interval is the server's own, out of the frame it opened with.
-- In this version of the protocol the client is the one that pings,
-- and a server that stops hearing them hangs up.
pinging :: Output Packet -> Int -> IO ()
pinging toSend interval = do
  threadDelay (interval * 1000)
  taken <- atomically (send toSend Ping)
  if taken then pinging toSend interval else pure ()
