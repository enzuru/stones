-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | The connection to online-go.com.
--
-- Nothing here reaches online-go.com. How to reach a server is an
-- argument to the socket, so these point it at one on this machine
-- that speaks the same protocol, and that server is also what says
-- what the client sent.
module SocketTest
  ( tests
  )
where

import           Control.Concurrent             ( threadDelay )
import qualified Control.Concurrent.Async      as Async
import           Control.Exception              ( SomeException
                                                , bracket
                                                , catch
                                                )
import           Data.Foldable                  ( traverse_ )
import           Data.IORef
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Hedgehog
import           Network.Socket                 ( Family(AF_INET)
                                                , SockAddr(SockAddrInet)
                                                , SocketType(Stream)
                                                , bind
                                                , close
                                                , defaultProtocol
                                                , socket
                                                , socketPort
                                                , tupleToHostAddress
                                                )
import qualified Network.WebSockets            as WebSockets
import           Pipes                          ( (>->) )
import qualified Pipes.Prelude                 as Pipes
import           System.Timeout                 ( timeout )

import           Stones.Ogs.Frame
import           Stones.Ogs.Socket

-- | The frame a server opens with, with this long between pings.
--
-- Short, because one of the tests below waits for the client to ping
-- on its own and a real quarter of a minute is a long test.
greeting :: Int -> Text
greeting interval =
  "0{\"sid\":\"test\",\"upgrades\":[],\"pingInterval\":"
    <> Text.pack (show interval)
    <> ",\"pingTimeout\":60000}"

-- | A port nothing is listening on.
freePort :: IO Int
freePort = bracket (socket AF_INET Stream defaultProtocol) close $ \listener -> do
  bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  fromIntegral <$> socketPort listener

-- | Run a server that opens with this and answers each frame with
-- these, and hand its port over along with a way to read what it was
-- sent.
--
-- What it answers with is text rather than packets, so that a test can
-- send something no packet can be written as.
withServer :: Text -> (Packet -> [Text]) -> (Int -> IO [Packet] -> IO a) -> IO a
withServer opening answering use = do
  port <- freePort
  seen <- newIORef []
  let application pending = do
        connection <- WebSockets.acceptRequest pending
        WebSockets.sendTextData connection opening
        let loop = do
              frame <- WebSockets.receiveData connection
              case decode frame of
                Right packet -> do
                  modifyIORef' seen (<> [packet])
                  traverse_ (WebSockets.sendTextData connection)
                            (answering packet)
                  loop
                Left _ -> loop
        loop `catch` \(_ :: SomeException) -> pure ()
  Async.withAsync (WebSockets.runServer "127.0.0.1" port application) $ \_ -> do
    -- Give it a moment to take the port.
    threadDelay 250000
    use port (readIORef seen)

-- | How the socket reaches a server on this machine.
dialing :: Int -> Dial
dialing port use = WebSockets.runClient "127.0.0.1" port "/socket.io/" use

-- | A server that answers a ping and echoes everything else.
ordinary :: Packet -> [Text]
ordinary = \case
  Ping  -> [encode Pong]
  other -> [encode other]

-- | Read this many things off a stream, or give up.
takeFrom :: Socket -> Int -> IO (Maybe [Either Text Packet])
takeFrom connection count =
  timeout 3000000 (Pipes.toListM (connection.heard >-> Pipes.take count))

prop_aConnectionComesUpAndCarriesWhatArrives :: Property
prop_aConnectionComesUpAndCarriesWhatArrives = withTests 1 . property $ do
  arrived <- evalIO . withServer (greeting 25000) ordinary $ \port _seen ->
    openWith (dialing port) >>= \case
      Left  why        -> pure (Left why)
      Right connection -> do
        _ <- connection.put (Event "hello" [] Nothing)
        answer <- takeFrom connection 1
        connection.stop
        pure (Right answer)
  arrived === Right (Just [Right (Event "hello" [] Nothing)])

prop_whatIsPutGoesOut :: Property
prop_whatIsPutGoesOut = withTests 1 . property $ do
  sent <- evalIO . withServer (greeting 25000) ordinary $ \port seen ->
    openWith (dialing port) >>= \case
      Left  _          -> pure []
      Right connection -> do
        _ <- connection.put (Event "game/connect" [] Nothing)
        _ <- connection.put (Event "game/move" [] (Just 7))
        _ <- takeFrom connection 2
        connection.stop
        seen
  sent === [Event "game/connect" [] Nothing, Event "game/move" [] (Just 7)]

prop_theClientPingsOnItsOwn :: Property
prop_theClientPingsOnItsOwn = withTests 1 . property $ do
  -- This version of the protocol has the client keeping the
  -- connection alive, and the opening frame is where the server says
  -- how often. Nothing here asks it to.
  sent <- evalIO . withServer (greeting 50) ordinary $ \port seen ->
    openWith (dialing port) >>= \case
      Left  _          -> pure []
      Right connection -> do
        threadDelay 600000
        connection.stop
        seen
  assert (Ping `elem` sent)

prop_aServerThatDoesNotOpenProperlyIsRefused :: Property
prop_aServerThatDoesNotOpenProperlyIsRefused = withTests 1 . property $ do
  -- A server that starts talking without saying how often to ping is
  -- not one this program knows how to keep a connection to.
  outcome <- evalIO . withServer "40" ordinary $ \port _seen ->
    openWith (dialing port) >>= \case
      Left  why        -> pure (Left why)
      Right connection -> Right () <$ connection.stop
  case outcome of
    Left why -> assert (Text.isInfixOf "opened with" why)
    Right () -> annotate "it took a connection it cannot keep" >> failure

prop_aServerThatIsNotThereIsRefused :: Property
prop_aServerThatIsNotThereIsRefused = withTests 1 . property $ do
  port    <- evalIO freePort
  outcome <- evalIO $ openWith (dialing port) >>= \case
    Left  why        -> pure (Left why)
    Right connection -> Right () <$ connection.stop
  case outcome of
    Left why -> assert (Text.isInfixOf "Lost the connection" why)
    Right () -> annotate "it reached a server that is not there" >> failure

prop_aFrameThatCannotBeReadArrivesAsAReason :: Property
prop_aFrameThatCannotBeReadArrivesAsAReason = withTests 1 . property $ do
  -- Dropping it quietly would leave the program waiting for something
  -- that already came and went.
  arrived <- evalIO . withServer (greeting 25000) (const ["42not a frame"]) $ \port _seen ->
    openWith (dialing port) >>= \case
      Left  _          -> pure Nothing
      Right connection -> do
        _      <- connection.put (Event "anything" [] Nothing)
        answer <- takeFrom connection 1
        connection.stop
        pure answer
  case arrived of
    Just [Left why] -> assert (not (Text.null why))
    other           -> annotateShow other >> failure

prop_aConnectionThatGoesSaysSoAndEnds :: Property
prop_aConnectionThatGoesSaysSoAndEnds = withTests 1 . property $ do
  -- The stream ending is how a game learns its opponent has gone, and
  -- the last thing on it is why.
  arrived <- evalIO . withServer (greeting 25000) (const [encode Close]) $ \port _seen ->
    openWith (dialing port) >>= \case
      Left  _          -> pure Nothing
      Right connection -> do
        _      <- connection.put (Event "goodbye" [] Nothing)
        answer <- timeout 3000000 (Pipes.toListM connection.heard)
        connection.stop
        pure answer
  case arrived of
    Just reasons -> do
      assert (not (null reasons))
      assert (all isLeft reasons)
    Nothing -> annotate "the stream never ended" >> failure
  where isLeft = either (const True) (const False)

tests :: Group
tests = $$(discover)
