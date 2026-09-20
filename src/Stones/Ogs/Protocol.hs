-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | What this program says to online-go.com about a game, and what
-- online-go.com says back.
--
-- All of it is a function of its arguments, so none of it needs a
-- socket. The socket carries frames, "Stones.Ogs.Frame" reads them,
-- and this turns what is inside one into something a game understands.
module Stones.Ogs.Protocol
  ( Table(..)
  , encodeMove
  , decodeMove
  , moverOf
  , outgoing
  , incoming
  )
where

import           Data.Aeson                     ( Value(..)
                                                , object
                                                , (.=)
                                                )
import qualified Data.Aeson.Types              as Aeson
import           Data.Scientific                ( toBoundedInteger )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Vector                   as Vector

import           Go.Types
import           Stones.Opponent                ( Heard(..)
                                                , Told(..)
                                                )

-- | The game this program is at, from the server's side of it.
data Table = Table
  { identifier :: Int
    -- ^ The number online-go.com knows this game by.
  , played     :: Int
    -- ^ How many moves have been made in it, which is what the server
    -- wants alongside anything said about the game.
  }
  deriving (Eq, Show)

-- | A move, as the server writes one.
--
-- Two letters, a column and a row, both counted from @a@. A pass is
-- two full stops. A resignation is not a move at all: it has an event
-- of its own.
encodeMove :: Move -> Text
encodeMove = \case
  Play (Coord x y) -> Text.pack [letter x, letter y]
  Pass             -> ".."
  Resign           -> ".."
  where letter n = toEnum (fromEnum 'a' + n)

-- | A move, as the server sends one: the column, the row, and how
-- long the player took. A pass is a column and row of minus one.
decodeMove :: Value -> Either Text Move
decodeMove = \case
  Array items -> case take 2 (Vector.toList items) of
    [x, y] -> do
      column <- whole x
      row    <- whole y
      if column < 0 || row < 0
        then Right Pass
        else Right (Play (Coord column row))
    _ -> Left "a move with no column and row in it"
  _ -> Left "a move that is not a column and a row"
 where
  whole (Number n) = maybe (Left "a move that is not whole") Right (toBoundedInteger n)
  whole _          = Left "a move that is not a number"

-- | Whose move that was.
--
-- The server says which move it was and not who made it, so it has to
-- be worked out, and the count is all there is to work it out from.
-- Black moves first, and this program asks for even games only, which
-- is what makes that safe. A handicap game gives Black several moves
-- before White has any, and every colour after them would be wrong.
moverOf :: Int -> Color
moverOf number = if even number then Black else White

-- | What to send when a game is told something.
--
-- Some of what a game says has nobody to say it to. The server is the
-- one keeping the rules, so it needs no telling whose turn it is, and
-- it runs the counting at the end itself.
outgoing :: Table -> Told -> [(Text, Value)]
outgoing table = \case
  Moved move ->
    [("game/move", object ["game_id" .= table.identifier, "move" .= encodeMove move])]
  Resigned -> [("game/resign", object ["game_id" .= table.identifier])]
  Said what ->
    [ ( "game/chat"
      , object
        [ "game_id" .= table.identifier
        , "type" .= ("main" :: Text)
        , "move_number" .= table.played
        , "body" .= what
        ]
      )
    ]
  TookBack count ->
    [ ( "game/undo/request"
      , object
        [ "game_id" .= table.identifier
        , "move_number" .= max 0 (table.played - count)
        ]
      )
    ]
  -- Nobody to say these to.
  Asked    -> []
  Counting -> []

-- | What to say to the game when an event arrives, if anything.
--
-- An event about another game, or about something this program does
-- not act on yet, is nothing to say.
incoming :: Table -> Text -> [Value] -> Maybe Heard
incoming table name arguments = do
  what <- Text.stripPrefix ("game/" <> Text.pack (show table.identifier) <> "/") name
  body <- case arguments of
    first : _ -> Just first
    []        -> Nothing
  case what of
    "move" -> do
      number <- field "move_number" body >>= asInt
      move   <- field "move" body
      Just (either Lost (Played (moverOf number)) (decodeMove move))
    "chat" -> do
      line   <- field "line" body
      person <- field "username" line >>= asText
      said   <- field "body" line >>= asText
      Just (Spoke person said)
    "undo_accepted" -> do
      number <- asInt body
      Just (Undid (max 0 (table.played - number)))
    _ -> Nothing
 where
  field key = \case
    Object fields -> Aeson.parseMaybe (Aeson..: key) fields
    _             -> Nothing
  asInt = \case
    Number n -> toBoundedInteger n
    _        -> Nothing
  asText = \case
    String text -> Just text
    _           -> Nothing
