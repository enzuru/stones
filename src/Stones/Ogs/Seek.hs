-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE NoFieldSelectors      #-}

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
import           Stones.Setup                   ( Handicapping(..)
                                                , Setup(..)
                                                , speedName
                                                )

-- | A game being looked for.
data Seek = Seek
  { name  :: Text
    -- ^ What this looking is called, which is how its answer is told
    -- from anybody else's.
  , wants :: Setup
    -- ^ What is being looked for.
  }
  deriving (Eq, Show)

-- | Start looking for a game of this shape.
newSeek :: Setup -> IO Seek
newSeek wants = do
  name <- newName
  pure Seek { name = name, wants = wants }

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
-- Japanese rules are asked for rather than insisted on, because they
-- settle a repeated position the way this program's own rules do, and
-- a game is better than a perfect match.
--
-- A handicap is not a number here. Nobody picks how many: the ranks
-- decide that, and the server says where a game starts when it is
-- joined, stones and all. What goes out is whether to be paired into
-- one at all, and how strongly.
finding :: Seek -> Packet
finding seek = Event
  "automatch/find_match"
  [ object
      [ "uuid" .= seek.name
      , "size_speed_options" .= ([option] :: [Value])
      , "lower_rank_diff" .= (3 :: Int)
      , "upper_rank_diff" .= (3 :: Int)
      , "rules" .= object
        ["condition" .= ("preferred" :: Text), "value" .= ("japanese" :: Text)]
      , "handicap" .= object ["condition" .= condition, "value" .= wanted]
      ]
  ]
  Nothing
 where
  -- Standard is what the server does when nobody says otherwise, so it
  -- is asked as a preference and the other two are insisted on: what
  -- they are for is ruling the other kind of game out.
  (condition, wanted) = case seek.wants.handicapping of
    Required -> ("required" :: Text, "enabled" :: Text)
    Standard -> ("no-preference", "enabled")
    Disabled -> ("required", "disabled")
  option = object
    [ "size" .= boardName seek.wants.size
    , "speed" .= speedName seek.wants.speed
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
