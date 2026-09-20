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
  ( Reaching(..)
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
import qualified Stones.Ogs.Login              as Login
import           Stones.Ogs.Seek                ( Seek(..)
                                                , Speed(..)
                                                )
import qualified Stones.Ogs.Seek               as Seek
import           Stones.Ogs.Socket              ( Socket(..) )
import qualified Stones.Ogs.Socket             as Socket
import           Stones.Opponent
import qualified Stones.Opponent.Ogs           as Ogs
import           Stones.Setup                   ( Setup(..) )

-- | Everything here that reaches off this machine.
--
-- Both are arguments, so that the whole of what follows can be driven
-- without a network: the signing in, because it opens a browser and
-- then asks online-go.com who came back, and the connection, because
-- it is a socket.
data Reaching = Reaching
  { signIn :: IO (Either Text Account)
  , dial   :: IO (Either Text Socket)
  }

-- | The real ones, with this for opening the browser.
reaching :: (Text -> IO ()) -> Reaching
reaching browse = Reaching
  { signIn = Login.login browse >>= \case
    Left  why    -> pure (Left why)
    -- Who the token belongs to, which is also what the connection is
    -- signed in with. The token itself is not kept: nothing after this
    -- point uses it.
    Right tokens -> Account.whoAmI tokens.access
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
withOnline :: (Text -> IO ()) -> (Opponents -> IO a) -> IO a
withOnline browse = withOnlineWith (reaching browse)

-- | The same, with what reaches outside handed in.
withOnlineWith :: Reaching -> (Opponents -> IO a) -> IO a
withOnlineWith outside use = do
  who  <- newIORef Nothing
  held <- newIORef []
  use
      Opponents { open  = finding outside who held
                , close = (.leave)
                , enter = entering outside who
                }
    `finally` (readIORef held >>= \open' -> for_ open' (.stop))

-- | Sign in, and remember who it was.
--
-- Signing in twice is signing in again: the browser is opened again
-- and whoever comes back is who the window is afterwards. That is what
-- a player who signed in as the wrong person would want.
entering :: Reaching -> IORef (Maybe Account) -> IO (Either Text Text)
entering outside who = outside.signIn >>= \case
  Left  why     -> pure (Left why)
  Right account -> Right account.username <$ writeIORef who (Just account)

-- | Look for a game, and play whatever is found.
finding
  :: Reaching
  -> IORef (Maybe Account)
  -> IORef [Socket]
  -> Setup
  -> IO (Either Text Opponent)
finding outside who held setup = readIORef who >>= \case
  Nothing      -> pure (Left "Sign in to online-go.com first.")
  Just account -> outside.dial >>= \case
    Left  why        -> pure (Left why)
    Right connection -> do
      modifyIORef' held (connection :)
      seek <- Seek.newSeek setup.size Live
      -- Whatever this stops for, the looking is called off: a game, a
      -- refusal, time running out, or the tab being closed while it
      -- waits. A looking left running is somebody else paired into a
      -- game with nobody at the other side of it.
      let givingUp = giveUp connection seek
      found <- pairing account connection seek `onException` givingUp
      case found of
        Left  why  -> Left why <$ givingUp
        Right them -> pure (Right them)

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
pairing :: Account -> Socket -> Seek -> IO (Either Text Opponent)
pairing account connection seek = Account.signingIn connection account >>= \case
  Left  why -> pure (Left why)
  Right ()  -> connection.put (Seek.finding seek) >>= \case
    Left  why -> pure (Left why)
    Right ()  -> waited >>= \case
      Nothing           -> pure (Left "Nobody else was looking for that game.")
      Just (Left  why ) -> pure (Left why)
      Just (Right game) -> Ogs.joining connection account.player game
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
