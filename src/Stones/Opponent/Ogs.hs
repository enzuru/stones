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
import           Data.Aeson                     ( object
                                                , (.=)
                                                )
import           Data.Foldable                  ( traverse_ )
import           Data.IORef
import           Data.Text                      ( Text )
import           Pipes                          ( Producer
                                                , for
                                                , yield
                                                )

import           Stones.Ogs.Frame               ( Packet(..) )
import           Stones.Ogs.Protocol
import           Stones.Ogs.Socket              ( Socket(..) )
import           Stones.Opponent

-- | Join a game on a connection that is already open.
--
-- The connection is not this game's to close, because the next game
-- is on the same one. Leaving says so and leaves the connection up.
joining :: Socket -> Int -> IO (Either Text Opponent)
joining connection game = do
  joined <- connection.put (connecting game)
  case joined of
    Left  why -> pure (Left why)
    Right ()  -> do
      -- How many moves are on the board, which is what the server
      -- wants alongside anything said about the game. It is counted
      -- rather than asked for, and every move is counted, this
      -- program's own included, because the server says them all.
      played <- newIORef 0
      pure
        (Right Opponent
          { name  = "online-go.com"
          , tell  = telling connection game played
          , hear  = hearing connection game played
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
telling :: Socket -> Int -> IORef Int -> Told -> IO (Either Text ())
telling connection game played told = do
  table  <- tableFor game played
  sent   <- traverse (connection.put . asEvent) (outgoing table told)
  pure (traverse_ id sent)
  where asEvent (name, body) = Event name [body] Nothing

-- | Everything about this game that arrives.
hearing :: Socket -> Int -> IORef Int -> Producer Heard IO ()
hearing connection game played = for connection.heard $ \case
  -- The connection has gone, and nothing follows it.
  Left  why                 -> yield (Lost why)
  Right (Event name args _) -> do
    table <- liftIO (tableFor game played)
    case incoming table name args of
      Nothing    -> pure ()
      Just heard -> do
        liftIO (counting played heard)
        yield heard
  -- Everything the connection needs for itself.
  Right _ -> pure ()

-- | Where the game is, for what is said about it next.
tableFor :: Int -> IORef Int -> IO Table
tableFor game played = Table game <$> readIORef played

-- | Keep count of the moves on the board.
counting :: IORef Int -> Heard -> IO ()
counting played = \case
  Played _ _ -> modifyIORef' played (+ 1)
  Undid n    -> modifyIORef' played (max 0 . subtract n)
  _          -> pure ()
