-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Games on online-go.com, as a window gets them.
--
-- The pieces this is made of are all here already: signing in, a
-- connection, looking for a game, and playing one. This is what runs
-- them in order, and what turns the answer into the same 'Opponents'
-- a window gets from a program on this machine, so that the window
-- does not know which it has.
--
-- Signing in happens once. Every game after that is played over a
-- connection of its own, because one connection carries one stream:
-- two games reading the same one would take each other's events, and a
-- window with two tabs would play half a game in each.
module Stones.Online
  ( Signed(..)
  , Reaching(..)
  , reaching
  , withOnline
  , withOnlineWith
  , waitingFor
  , patience
  , browsing
  )
where

import           Control.Exception              ( SomeException
                                                , finally
                                                , onException
                                                , try
                                                )
import           Control.Monad                  ( void )
import           Data.Foldable                  ( for_ )
import           Data.IORef
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Pipes
import           System.Process                 ( callProcess )
import qualified System.Timeout                as Timeout

import           Stones.Ogs.Account             ( Account(..) )
import qualified Stones.Ogs.Account            as Account
import           Stones.Ogs.Frame               ( Packet(..) )
import qualified Stones.Ogs.Challenge          as Challenge
import qualified Stones.Ogs.Login              as Login
import           Stones.Ogs.Seek                ( Seek(..)
                                                , Speed(..)
                                                )
import qualified Stones.Ogs.Seek               as Seek
import           Stones.Ogs.Socket              ( Socket(..) )
import qualified Stones.Ogs.Socket             as Socket
import           Stones.Log                     ( Log(..)
                                                , watching
                                                )
import           Stones.Opponent
import qualified Stones.Opponent.Ogs           as Ogs
import           Stones.Setup                   ( Setup(..)
                                                , versusOf
                                                )

-- | Everything here that reaches off this machine.
--
-- Both are arguments, so that the whole of what follows can be driven
-- without a network: the signing in, because it opens a browser and
-- then asks online-go.com who came back, and the connection, because
-- it is a socket.
data Reaching = Reaching
  { signIn :: IO (Either Text Signed)
  , dial   :: IO (Either Text Socket)
  }

-- | Somebody signed in, and what it takes to act as them.
--
-- The token is kept as well as the account, because asking a named
-- player for a game is a call to the website rather than something
-- said over the connection, and that call is made with the token.
data Signed = Signed
  { account :: Account
  , token   :: Text
  }

-- | The real ones, with this for opening the browser.
reaching :: (Text -> IO ()) -> Reaching
reaching browse = Reaching
  { signIn = Login.login browse >>= \case
    Left  why    -> pure (Left why)
    -- Who the token belongs to, which is also what the connection is
    -- signed in with.
    Right tokens -> Account.whoAmI tokens.access >>= \case
      Left  why     -> pure (Left why)
      Right account -> pure (Right (Signed account tokens.access))
  , dial   = Socket.open
  }

-- | Open an address in whatever the desktop opens addresses in.
--
-- This is the one place the program hands something to the rest of the
-- machine. A desktop with no handler set up says so on the terminal
-- and the signing in waits at a port nobody knocks on, which is what
-- the waiting is for.
browsing :: Text -> IO ()
browsing address = do
  tried <- try (callProcess "xdg-open" [Text.unpack address])
  case tried of
    Right ()                      -> pure ()
    Left (_ :: SomeException) -> putStrLn
      ("Stones: could not open a browser. Go to " <> Text.unpack address)

-- | How long to wait for a game before giving up on one.
--
-- Being paired waits for somebody else looking for the same game at
-- the same time, which on a quiet board takes a while. Long enough
-- that a real pairing is not thrown away, and short enough that a tab
-- somebody walked away from does not hold a looking open all day.
patience :: Int
patience = 10 * 60

-- | Games on online-go.com, for as long as this action runs.
withOnline :: Log -> (Text -> IO ()) -> (Opponents -> IO a) -> IO a
withOnline log' browse = withOnlineWith log' (reaching browse)

-- | The same, with what reaches outside handed in.
withOnlineWith :: Log -> Reaching -> (Opponents -> IO a) -> IO a
withOnlineWith log' outside use = do
  who  <- newIORef Nothing
  held <- newIORef []
  use
      Opponents { open  = finding log' outside who held
                , close = (.leave)
                , enter = entering log' outside who
                }
    `finally` (readIORef held >>= \open' -> for_ open' (.stop))

-- | Sign in, and remember who it was.
--
-- Signing in twice is signing in again: the browser is opened again
-- and whoever comes back is who the window is afterwards. That is what
-- a player who signed in as the wrong person would want.
entering :: Log -> Reaching -> IORef (Maybe Signed) -> IO (Either Text Text)
entering log' outside who = outside.signIn >>= \case
  Left why -> Left why <$ log'.note ("sign in refused: " <> why)
  Right signed -> do
    log'.note
      (  "signed in as "
      <> signed.account.username
      <> ", player "
      <> Text.pack (show signed.account.player)
      )
    writeIORef who (Just signed)
    pure (Right signed.account.username)

-- | Look for a game, and play whatever is found.
finding
  :: Log
  -> Reaching
  -> IORef (Maybe Signed)
  -> IORef [Socket]
  -> Setup
  -> IO (Either Text Opponent)
finding log' outside who held setup = readIORef who >>= \case
  Nothing     -> pure (Left "Sign in to online-go.com first.")
  Just signed -> outside.dial >>= \case
    Left why -> Left why <$ log'.note ("could not connect: " <> why)
    Right dialled -> do
      let connection = watching log' dialled
      modifyIORef' held (connection :)
      log'.note
        (  "asking for a "
        <> Text.pack (show setup.size)
        <> "x"
        <> Text.pack (show setup.size)
        <> " game against "
        <> (if Text.null (versusOf setup) then "anybody" else versusOf setup)
        )
      found <- if Text.null setup.versus
        then looking log' signed connection setup
        else asked log' signed connection setup
      case found of
        Left  why -> Left why <$ log'.note ("no game: " <> why)
        Right it  -> pure (Right it)

-- | A game against whoever else is looking for the same one.
looking :: Log -> Signed -> Socket -> Setup -> IO (Either Text Opponent)
looking log' signed connection setup = do
  seek <- Seek.newSeek setup.size Live
  -- Whatever this stops for, the looking is called off: a game, a
  -- refusal, time running out, or the tab being closed while it
  -- waits. A looking left running is somebody else paired into a
  -- game with nobody at the other side of it.
  let givingUp = giveUp connection seek
  found <- pairing log' signed.account connection seek `onException` givingUp
  case found of
    Left  why  -> Left why <$ givingUp
    Right them -> pure (Right them)

-- | A game against somebody in particular.
--
-- Asking a named player is a call to the website rather than something
-- said over the connection, and it answers with the game there and
-- then, so there is nothing to wait for and nothing to call off. What
-- there is to wait for is the other side accepting, which the game
-- itself shows.
asked :: Log -> Signed -> Socket -> Setup -> IO (Either Text Opponent)
asked log' signed connection setup =
  Challenge.findPlayer signed.token (versusOf setup) >>= \case
    Left  why -> Left why <$ connection.stop
    Right who -> do
      log'.note (versusOf setup <> " is player " <> Text.pack (show who))
      Challenge.challenging signed.token who setup >>= \case
        Left  why     -> Left why <$ connection.stop
        Right started -> do
          log'.note
            (  "challenge "
            <> Text.pack (show started.challenge)
            <> " made game "
            <> Text.pack (show started.game)
            )
          Account.signingIn connection signed.account >>= \case
            Left why -> Left why <$ connection.stop
            Right () -> Ogs.joining log'
                                    connection
                                    signed.account.player
                                    started.game

-- | Stop looking, and let the connection go.
--
-- The looking is called off before the connection closes rather than
-- by closing it, because a connection that goes away without a word is
-- a connection the server may wait on.
giveUp :: Socket -> Seek -> IO ()
giveUp connection seek = do
  void (connection.put (Seek.cancelling seek))
  connection.stop

-- | Sign the connection in, ask for a game, and wait for one.
pairing :: Log -> Account -> Socket -> Seek -> IO (Either Text Opponent)
pairing log' account connection seek = Account.signingIn connection account >>= \case
  Left  why -> pure (Left why)
  Right ()  -> connection.put (Seek.finding seek) >>= \case
    Left  why -> pure (Left why)
    Right ()  -> waited >>= \case
      Nothing           -> pure (Left "Nobody else was looking for that game.")
      Just (Left  why ) -> pure (Left why)
      Just (Right game) -> Ogs.joining log' connection account.player game
  where waited = Timeout.timeout (patience * 1000000) (waitingFor connection seek)

-- | Wait for the game this looking is answered with.
--
-- Everything else that arrives while waiting is somebody else's, and
-- goes by. What is left in the stream when this stops is what the game
-- reads next, because the game reads the same connection.
waitingFor :: Socket -> Seek -> IO (Either Text Int)
waitingFor connection seek = go connection.heard
 where
  go stream = Pipes.next stream >>= \case
    Left ()                   -> pure (Left gone)
    Right (Left why    , _   ) -> pure (Left why)
    Right (Right packet, rest) -> case packet of
      Event name arguments _ | Just game <- Seek.started seek name arguments ->
        pure (Right game)
      _ -> go rest
  gone = "online-go.com went away while looking for a game."
