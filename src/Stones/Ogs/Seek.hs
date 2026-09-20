-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Looking for somebody to play against.
--
-- A seek says what kind of game is wanted and then waits. The server
-- pairs it with somebody else who asked for something that fits, and
-- says so with the number of the game it made, which is the number
-- "Stones.Opponent.Ogs" joins.
--
-- The name on a seek is what tells one from another, because the
-- answer names the seek it answers rather than the person who made it.
module Stones.Ogs.Seek
  ( Seek(..)
  , Speed(..)
  , speeds
  , describeSpeed
  , newSeek
  , finding
  , cancelling
  , started
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
import           System.Random                  ( randomRIO )

import           Stones.Ogs.Frame               ( Packet(..) )

-- | How long a game is allowed to take.
data Speed
  = Blitz
  | Rapid
  | Live
  | Correspondence
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Every speed, quickest first.
speeds :: [Speed]
speeds = [minBound .. maxBound]

-- | What the server calls one, which is also what to call it in the
-- window.
describeSpeed :: Speed -> Text
describeSpeed = \case
  Blitz          -> "blitz"
  Rapid          -> "rapid"
  Live           -> "live"
  Correspondence -> "correspondence"

-- | A game being looked for.
data Seek = Seek
  { name  :: Text
    -- ^ What this looking is called, which is how its answer is told
    -- from anybody else's.
  , size  :: Int
  , speed :: Speed
  }
  deriving (Eq, Show)

-- | Start looking for a game of this shape.
newSeek :: Int -> Speed -> IO Seek
newSeek size speed = do
  name <- newName
  pure Seek { name = name, size = size, speed = speed }

-- | A name no other looking is likely to have.
newName :: IO Text
newName = do
  letters <- traverse (const (pick "0123456789abcdef")) [1 .. 32 :: Int]
  let (a, rest)  = splitAt 8 letters
      (b, rest') = splitAt 4 rest
      (c, rest'') = splitAt 4 rest'
      (d, e)     = splitAt 4 rest''
  pure (Text.pack (a <> "-" <> b <> "-" <> c <> "-" <> d <> "-" <> e))
  where pick alphabet = (alphabet !!) <$> randomRIO (0, length alphabet - 1)

-- | The frame that asks for a game.
--
-- Handicap is refused rather than left open. The server says which
-- move arrived and not who made it, so this program works the colour
-- out from the count, and a handicap game gives Black several moves
-- before White has any, which would make every colour after them
-- wrong. Japanese rules are asked for rather than required, because
-- they settle a repeated position the way this program's own rules do
-- and a game is better than a perfect match.
finding :: Seek -> Packet
finding seek = Event
  "automatch/find_match"
  [ object
      [ "uuid" .= seek.name
      , "size_speed_options" .= [option]
      , "lower_rank_diff" .= (3 :: Int)
      , "upper_rank_diff" .= (3 :: Int)
      , "rules" .= object
        ["condition" .= ("preferred" :: Text), "value" .= ("japanese" :: Text)]
      , "handicap" .= object
        ["condition" .= ("required" :: Text), "value" .= ("disabled" :: Text)]
      ]
  ]
  Nothing
 where
  option = object
    [ "size" .= boardName seek.size
    , "speed" .= describeSpeed seek.speed
    , "system" .= ("byoyomi" :: Text)
    ]

-- | What the server calls a board that wide.
boardName :: Int -> Text
boardName size = let n = Text.pack (show size) in n <> "x" <> n

-- | The frame that stops looking.
cancelling :: Seek -> Packet
cancelling seek = Event "automatch/cancel" [object ["uuid" .= seek.name]] Nothing

-- | The game this looking was answered with, if this event is its
-- answer.
started :: Seek -> Text -> [Value] -> Maybe Int
started seek event arguments = do
  body <- case (event, arguments) of
    ("automatch/start", first : _) -> Just first
    _                              -> Nothing
  answered <- field "uuid" body >>= asText
  if answered /= seek.name then Nothing else field "game_id" body >>= asInt
 where
  field key = \case
    Object fields -> Aeson.parseMaybe (Aeson..: key) fields
    _             -> Nothing
  asText = \case
    String text -> Just text
    _           -> Nothing
  asInt = \case
    Number n -> toBoundedInteger n
    _        -> Nothing
