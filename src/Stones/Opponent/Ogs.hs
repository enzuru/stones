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
import qualified Data.Text                     as Text
import           Pipes                          ( Producer
                                                , for
                                                , next
                                                , yield
                                                )
import qualified System.Timeout                as Timeout

import           Go.Types                       ( Color(..) )
import           Stones.Ogs.Frame               ( Packet(..) )
import           Stones.Ogs.Protocol
import           Stones.Log                     ( Log(..) )
import           Stones.Ogs.Socket              ( Socket(..) )
import           Stones.Opponent

-- | Join a game on a connection that is already open.
--
-- The connection is not this game's to close, because the next game
-- is on the same one. Leaving says so and leaves the connection up.
joining :: Log -> Socket -> Int -> Int -> IO (Either Text Opponent)
joining log' connection player game = do
  joined <- connection.put (connecting game)
  case joined of
    Left  why -> pure (Left why)
    Right ()  -> do
      -- Where the game is, as far as this side knows. The count and
      -- who opened are both guesses until the server says.
      table <- newIORef Table { identifier = game
                              , played     = 0
                              , opens      = Black
                              , me         = player
                              }
      -- Nothing is handed back until the server has said where the
      -- game stands, the same way a game against a program on this
      -- machine is set up before anybody is given one to play. The
      -- board here is a guess until then, and a window that let
      -- somebody move on a guess would be sending moves to a game
      -- that might not even exist.
      opened <- beginning log' connection table game
      pure $ case opened of
        Left  why            -> Left why
        Right (began, theirs) -> Right Opponent
          { name  = theirs
          , tell  = telling connection table
            -- Where it stands was read off the stream to get here, so
            -- it is put back at the front of what the game hears.
          , hear  = yield began >> hearing log' connection table
          , leave = void (connection.put (leaving game))
          , talks = True
            -- Asking for them by name again is a rematch, which is
            -- what the website offers at the end of a game, and which
            -- they are free to turn down.
          , again = Just theirs
          }

-- | How long to wait for the server to say where a game stands.
--
-- A game asked for by name waits on the other side accepting it, which
-- a program does at once and a person takes as long as they like over.
-- Long enough for either, and short enough that a tab does not sit on
-- "Starting" forever with nothing coming.
patience :: Int
patience = 120 * 1000000

-- | Wait for the server to say where the game stands.
--
-- Three ways out. It says, and the game can be played. It says the
-- other side would not play this game, which is news that arrives as a
-- notice rather than as anything about the game, because the game is
-- now never going to exist. Or nothing comes at all.
--
-- What arrives before it and is not either of those is dropped. Only
-- the server's own housekeeping comes that early, and the game has not
-- started yet, so there is nothing yet for anything to be about.
beginning
  :: Log -> Socket -> IORef Table -> Int -> IO (Either Text (Heard, Text))
beginning log' connection table game =
  Timeout.timeout patience (go connection.heard) >>= \case
    Nothing     -> pure (Left "online-go.com never said where the game stands.")
    Just answer -> pure answer
 where
  go stream = next stream >>= \case
    Left  ()                   -> pure (Left gone)
    Right (Left why    , _   ) -> pure (Left why)
    Right (Right packet, rest) -> case packet of
      Event name arguments _ -> do
        where' <- readIORef table
        case incoming where' name arguments of
          Just began@(Began _) -> do
            log'.note ("    " <> name <> " -> " <> Text.pack (show began))
            following table arguments began
            -- Who it is against comes out of the same payload. A game
            -- somebody was paired into is against somebody nobody here
            -- has heard of until now.
            let theirs = case arguments of
                  body : _ -> maybe "online-go.com" id (opponentIn where'.me body)
                  []       -> "online-go.com"
            log'.note ("    playing " <> theirs)
            pure (Right (began, theirs))
          Just (Lost why) -> pure (Left why)
          _               -> case refusalOf game name arguments of
            Just why -> Left why <$ log'.note ("refused: " <> why)
            Nothing  -> go rest
      _ -> go rest
  gone = "online-go.com went away before the game started."

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
hearing :: Log -> Socket -> IORef Table -> Producer Heard IO ()
hearing log' connection table = for connection.heard $ \case
  -- The connection has gone, and nothing follows it.
  Left  why                 -> yield (Lost why)
  Right (Event name args _) -> do
    where' <- liftIO (readIORef table)
    case incoming where' name args of
      -- Either it is about another game, or it is something this
      -- program has nothing to do with. Which of the two it is is
      -- worth writing down: the second is a gap, and the first is not.
      Nothing -> liftIO (log'.note ("    nothing to do with " <> name))
      Just heard -> do
        liftIO (log'.note ("    " <> name <> " -> " <> Text.pack (show heard)))
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
