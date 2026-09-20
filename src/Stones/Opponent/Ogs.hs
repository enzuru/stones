-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | A game on online-go.com, as an opponent like any other.
--
-- This is where the three pieces meet. "Stones.Ogs.Socket" holds the
-- connection up, "Stones.Ogs.Frame" reads the frames off it, and
-- "Stones.Ogs.Protocol" says what is inside one. What is left is
-- joining the two halves of the seam to them: what a game is told
-- goes out as events, and what arrives comes back as things heard.
--
-- One connection carries every game at once, so joining is done per
-- game and the name on each event is what says which game it belongs
-- to.
module Stones.Opponent.Ogs
  ( joining
  )
where

import           Control.Monad                  ( void )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson                     ( Value
                                                , object
                                                , (.=)
                                                )
import           Data.Foldable                  ( traverse_ )
import           Data.IORef
import           Data.Text                      ( Text )
import           Pipes                          ( Producer
                                                , for
                                                , yield
                                                )

import           Go.Types                       ( Color(..) )
import           Stones.Ogs.Frame               ( Packet(..) )
import           Stones.Ogs.Protocol
import           Stones.Ogs.Socket              ( Socket(..) )
import           Stones.Opponent

-- | Join a game on a connection that is already open.
--
-- The connection is not this game's to close, because the next game
-- is on the same one. Leaving says so and leaves the connection up.
joining :: Socket -> Int -> Int -> IO (Either Text Opponent)
joining connection player game = do
  joined <- connection.put (connecting game)
  case joined of
    Left  why -> pure (Left why)
    Right ()  -> do
      -- Where the game is, as far as this side knows. The count and
      -- who opened are both guesses until the server says, which it
      -- does before anything else happens.
      table <- newIORef Table { identifier = game
                              , played     = 0
                              , opens      = Black
                              , me         = player
                              }
      pure
        (Right Opponent
          { name  = "online-go.com"
          , tell  = telling connection table
          , hear  = hearing connection table
          , leave = void (connection.put (leaving game))
          }
        )

-- | Say that this program is watching this game, and would like what
-- is said about it as well.
connecting :: Int -> Packet
connecting game = Event
  "game/connect"
  [object ["game_id" .= game, "chat" .= True]]
  Nothing

-- | Say that it is not watching any more.
leaving :: Int -> Packet
leaving game = Event "game/disconnect" [object ["game_id" .= game]] Nothing

-- | Say one thing about the game.
--
-- Something with nobody to say it to goes nowhere and counts as said,
-- because it was said to everyone there was to say it to.
telling :: Socket -> IORef Table -> Told -> IO (Either Text ())
telling connection table told = do
  where'  <- readIORef table
  sent    <- traverse (connection.put . asEvent) (outgoing where' told)
  pure (traverse_ id sent)
  where asEvent (name, body) = Event name [body] Nothing

-- | Everything about this game that arrives.
hearing :: Socket -> IORef Table -> Producer Heard IO ()
hearing connection table = for connection.heard $ \case
  -- The connection has gone, and nothing follows it.
  Left  why                 -> yield (Lost why)
  Right (Event name args _) -> do
    where' <- liftIO (readIORef table)
    case incoming where' name args of
      Nothing    -> pure ()
      Just heard -> do
        liftIO (following table args heard)
        yield heard
  -- Everything the connection needs for itself.
  Right _ -> pure ()

-- | Keep up with where the game is.
--
-- What the server says when the game is joined settles both of the
-- things that were guesses: how far along it is, and who moved first,
-- which is White in a handicap game because Black's stones are down
-- before anybody has moved.
following :: IORef Table -> [Value] -> Heard -> IO ()
following table arguments = \case
  Began _ -> case arguments of
    body : _ -> modifyIORef' table $ \where' -> where'
      { played = maybe where'.played id (countFrom body)
      , opens  = maybe where'.opens id (openerFrom body)
      }
    [] -> pure ()
  Played _ _ -> modifyIORef' table (\where' -> where' { played = where'.played + 1 })
  Undid n ->
    modifyIORef' table (\where' -> where' { played = max 0 (where'.played - n) })
  _ -> pure ()
