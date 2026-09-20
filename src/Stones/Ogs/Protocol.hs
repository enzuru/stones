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
  , countFrom
  , openerFrom
  , encodeMove
  , decodeMove
  , decodeStones
  , encodeStones
  , resultFrom
  , moverOf
  , openingFrom
  , clockFrom
  , timeFrom
  , outgoing
  , incoming
  )
where

import           Data.Aeson                     ( Value(..)
                                                , object
                                                , (.=)
                                                )
import qualified Data.Aeson.Key                as Key
import qualified Data.Aeson.Types              as Aeson
import           Data.Scientific                ( toBoundedInteger
                                                , toRealFloat
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Vector                   as Vector

import           Go.Board                       ( coords
                                                , stoneAt
                                                )
import           Go.Game
import           Go.Types
import           Stones.Opponent                ( Clock(..)
                                                , Heard(..)
                                                , Opening(..)
                                                , Time(..)
                                                , Told(..)
                                                )

-- | A field of an object.
fieldOf :: Text -> Value -> Maybe Value
fieldOf key = \case
  Object fields -> Aeson.parseMaybe (Aeson..: Key.fromText key) fields
  _             -> Nothing

-- | A whole number, if that is what this is.
asWhole :: Value -> Maybe Int
asWhole = \case
  Number n -> toBoundedInteger n
  _        -> Nothing

-- | A number of seconds, rounded down.
--
-- Times come back fractional, and part of a second is not worth
-- showing. Down rather than to the nearest, because a clock that says
-- a second more than there is says the wrong thing at the end, which
-- is the end that matters.
asSeconds :: Value -> Maybe Int
asSeconds = \case
  Number n -> Just (max 0 (floor (toRealFloat n :: Double)))
  _        -> Nothing

-- | Text, if that is what this is.
asString :: Value -> Maybe Text
asString = \case
  String text -> Just text
  _           -> Nothing

-- | A list, if that is what this is.
asList :: Value -> Maybe [Value]
asList = \case
  Array items -> Just (Vector.toList items)
  _           -> Nothing

-- | A colour, as the server writes one.
asColor :: Value -> Either Text Color
asColor = \case
  String "black" -> Right Black
  String "white" -> Right White
  _              -> Left "a colour that is neither black nor white"

-- | The game this program is at, from the server's side of it.
data Table = Table
  { identifier :: Int
    -- ^ The number online-go.com knows this game by.
  , played     :: Int
    -- ^ How many moves have been made in it, which is what the server
    -- wants alongside anything said about the game.
  , opens      :: Color
    -- ^ Who made the first move. Black in an even game, and White in a
    -- handicap one, where Black's stones are down before anybody has
    -- moved at all.
  , me         :: Int
    -- ^ The number the server knows the player at this window by,
    -- which is what says which colour they have.
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

-- | The stones in one of the strings a game starts with: two letters
-- each, a column and a row, both counted from @a@, with nothing
-- between them. Anything left over at the end is not a stone.
decodeStones :: Text -> [Coord]
decodeStones text = case Text.unpack (Text.take 2 text) of
  [column, row] | Just x <- number column, Just y <- number row ->
    Coord x y : decodeStones (Text.drop 2 text)
  _ -> []
 where
  number letter =
    let n = fromEnum letter - fromEnum 'a'
    in  if n >= 0 && n < 26 then Just n else Nothing

-- | Stones as the server writes a set of them: two letters each, with
-- nothing between them.
encodeStones :: [Coord] -> Text
encodeStones stones =
  Text.concat [ encodeMove (Play stone) | stone <- stones ]

-- | What a game came to, out of what the server says about it once it
-- has finished.
--
-- Written the way a score is written everywhere else: the winner's
-- initial, a plus, and what they won by. The server gives the winner
-- as a player number rather than as a colour, so it is matched against
-- the two players it names alongside.
resultFrom :: Value -> Maybe Text
resultFrom body = do
  outcome <- fieldOf "outcome" body >>= asString
  winner  <- fieldOf "winner" body >>= asWhole
  black   <- fieldOf "black_player_id" body >>= asWhole
  let side = if winner == black then "B" else "W"
  Just (side <> "+" <> outcome)

-- | Whose move that was, in a game where this colour moved first.
--
-- The server says which move it was and not who made it, so it has to
-- be worked out, and the count is all there is to work it out from.
-- Which colour starts is not always Black: a handicap game has Black's
-- stones down before anybody moves, so White moves first.
moverOf :: Color -> Int -> Color
moverOf first number = if even number then first else opposite first

-- | Where a game stands, out of everything the server says about it
-- when it is joined.
--
-- The moves so far are played out rather than taken as read, because
-- what is wanted is the position they lead to and stones come off
-- along the way. A move these rules will not take means this program
-- and the server disagree about the game, which is worth saying now
-- rather than later.
openingFrom :: Int -> Value -> Either Text Opening
openingFrom player body = do
  width   <- need "width" >>= must "a width that is not a number" . asWhole
  first   <- need "initial_player" >>= asColor
  black   <- need "black_player_id"
    >>= must "a black player that is not a number" . asWhole
  let start = fieldOf "initial_state" body
      down  =  [ (Black, stone) | stone <- placed start "black" ]
            <> [ (White, stone) | stone <- placed start "white" ]
  made    <- traverse decodeMove (maybe [] id (fieldOf "moves" body >>= asList))
  reached <- playOut (gameFrom width down first) made
  Right Opening
    { width  = width
    , stones = standing reached.board
    , opens  = reached.turn
    , yours  = if player == black then Black else White
    }
 where
  need key = maybe (Left ("a game with no " <> key <> " in it")) Right
                   (fieldOf key body)
  must why = maybe (Left why) Right
  placed start which = maybe [] decodeStones (start >>= fieldOf which >>= asString)
  playOut game []             = Right game
  playOut game (move : after) = case playMove game.turn move game of
    Left  reason -> Left ("a move these rules refuse: " <> describeIllegal reason)
    Right next   -> playOut next after
  standing board =
    [ (color, point) | point <- coords board, Just color <- [stoneAt board point] ]

-- | One player's clock, out of what the server says about it.
--
-- A game can be counted in several ways and the server writes each of
-- them differently. A plain number is a game with one lot of time and
-- nothing after it. An object holds the time to think in, and, if the
-- game has them, the short periods that follow. The parts of the other
-- ways that do not fit are left out rather than guessed at: what is
-- shown is then the time that is running, which is the part a player
-- is watching.
timeFrom :: Value -> Maybe Time
timeFrom = \case
  Number n -> do
    seconds <- asSeconds (Number n)
    Just Time { seconds = seconds, periods = 0, each = 0 }
  body@(Object _) -> do
    seconds <- fieldOf "thinking_time" body >>= asSeconds
    Just Time
      { seconds = seconds
      , periods = maybe 0 id (fieldOf "periods" body >>= asWhole)
      , each    = maybe 0 id (fieldOf "period_time" body >>= asSeconds)
      }
  _ -> Nothing

-- | Both clocks, out of what the server says when one of them moves.
--
-- Whose is going down is worked out from who the server says is to
-- play, against the two players it names, rather than from whose turn
-- this program thinks it is. A game nobody is waiting on, because it
-- is paused or has not started, has neither running.
clockFrom :: Value -> Maybe Clock
clockFrom body = do
  black <- fieldOf "black_time" body >>= timeFrom
  white <- fieldOf "white_time" body >>= timeFrom
  Just Clock { black = black, white = white, running = whose }
 where
  whose = case fieldOf "paused_since" body of
    Just (Number _) -> Nothing
    _               -> do
      player <- fieldOf "current_player" body >>= asWhole
      let sideOf key = fieldOf key body >>= asWhole
      if sideOf "black_player_id" == Just player
        then Just Black
        else if sideOf "white_player_id" == Just player
          then Just White
          else Nothing

-- | How far along a game is, out of what the server says about it.
countFrom :: Value -> Maybe Int
countFrom body = length <$> (fieldOf "moves" body >>= asList)

-- | Who moved first in it, out of the same.
openerFrom :: Value -> Maybe Color
openerFrom body =
  fieldOf "initial_player" body >>= either (const Nothing) Just . asColor

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
  Marking stones dead ->
    [ ( "game/removed_stones/set"
      , object
        [ "game_id" .= table.identifier
        , "stones" .= encodeStones stones
        , "removed" .= dead
        ]
      )
    ]
  Accepting stones ->
    [ ( "game/removed_stones/accept"
      , object
        [ "game_id" .= table.identifier
        , "stones" .= encodeStones stones
          -- Seki is counted the ordinary way, which is what the server
          -- does when nobody says otherwise.
        , "strict_seki_mode" .= False
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
      Just (either Lost (Played (moverOf table.opens number)) (decodeMove move))
    "chat" -> do
      line   <- field "line" body
      person <- field "username" line >>= asText
      said   <- field "body" line >>= asText
      Just (Spoke person said)
    -- A game that has finished sends the whole of itself again, with
    -- what it came to in it. Reading that as a game starting would put
    -- the board back up and throw the result away.
    "gamedata" -> case fieldOf "phase" body >>= asString of
      Just "finished" -> Counted <$> resultFrom body
      _               -> Just (either Lost Began (openingFrom table.me body))
    "clock" -> Ticked <$> clockFrom body
    -- A game that has finished says so twice: once here, with nothing
    -- but the word, and once as the whole game again with the result
    -- in it. The second is the one acted on, because it is the one
    -- that says what the game came to. If it turns out not to arrive,
    -- a game would sit here showing a finished board with an Accept
    -- button that does nothing, which is wrong but is not a lie.
    "phase" -> case asString body of
      Just "stone removal" -> Just Stopped
      _                    -> Nothing
    -- The whole set of dead stones rather than what changed, so a
    -- window that missed one is not left showing a board nobody else
    -- can see. An event without the whole set in it is nothing to act
    -- on, because a set worked out from a change is a set that can be
    -- wrong, and a wrong one would have this player agreeing to a
    -- count they cannot see.
    "removed_stones" ->
      Removing . decodeStones <$> (fieldOf "all_removed" body >>= asString)
    "undo_accepted" -> do
      number <- asInt body
      Just (Undid (max 0 (table.played - number)))
    _ -> Nothing
 where
  field = fieldOf
  asInt = asWhole
  asText = asString
