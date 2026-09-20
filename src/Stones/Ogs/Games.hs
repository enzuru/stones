-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | The games this player is already in.
--
-- A player is in games this window has never heard of: ones started on
-- the website, ones a tab was closed on, ones that have been waiting
-- days for a move. They are asked for rather than waited for, because
-- the answer is a list of what is true now.
module Stones.Ogs.Games
  ( ongoingFrom
  , myGames
  , address
  )
where

import           Data.Aeson                     ( Value(..)
                                                , eitherDecodeStrict
                                                )
import qualified Data.Aeson.Key                as Key
import qualified Data.Aeson.Types              as Aeson
import           Data.Bifunctor                 ( first )
import qualified Data.ByteString               as ByteString
import           Data.Maybe                     ( listToMaybe
                                                , mapMaybe
                                                )
import           Data.Scientific                ( toBoundedInteger )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Vector                   as Vector

import           Stones.Ogs.Challenge           ( calling )
import           Stones.Opponent                ( Ongoing(..) )
import           Stones.Setup                   ( Against(..)
                                                , Setup(..)
                                                , Speed(..)
                                                , defaultSetup
                                                , speedName
                                                , speeds
                                                )

-- | Where to ask for them.
--
-- Games that have not ended, newest first, and as many of them as
-- anybody is realistically in at once. Somebody in more than that has
-- a website to look at.
address :: Int -> Text
address player =
  "https://online-go.com/api/v1/players/"
    <> Text.pack (show player)
    <> "/games/?ended__isnull=true&ordering=-id&page_size=50"

-- | Ask for them.
myGames :: Text -> Int -> IO (Either Text [Ongoing])
myGames token player =
  calling token (address player) Nothing (ongoingFrom player)

-- | What came back, as games this window could open.
--
-- A game this program cannot play is left out rather than listed and
-- then failed at: a board that is not square is not a board these
-- rules are written for, and a game with nobody's name on it is one
-- there would be nothing to call.
ongoingFrom :: Int -> ByteString.ByteString -> Either Text [Ongoing]
ongoingFrom player body = do
  answer <- first (("online-go.com answered with " <>) . Text.pack)
                  (eitherDecodeStrict body)
  case fieldOf "results" answer of
    Just (Array found) ->
      Right (mapMaybe (oneOf player) (Vector.toList found))
    _ -> Left "online-go.com sent no list of games"

-- | One of them.
oneOf :: Int -> Value -> Maybe Ongoing
oneOf player body = do
  game   <- fieldOf "id" body >>= asWhole
  width  <- fieldOf "width" body >>= asWhole
  height <- fieldOf "height" body >>= asWhole
  black  <- fieldOf "black" body >>= asWhole
  both   <- fieldOf "players" body
  let mine   = if player == black then "black" else "white"
      theirs = if player == black then "white" else "black"
  them <- fieldOf theirs both >>= fieldOf "username" >>= asString
  if width /= height
    then Nothing
    else Just Ongoing
      { with  = them
      , game  = game
      , about = defaultSetup
        { against  = Online
        , size     = width
          -- Which colour this player has in it, so the board is the
          -- right way round before the server says so.
        , human    = if mine == "black" then minBound else maxBound
        , ranked   = maybe False id (fieldOf "ranked" body >>= asBool)
        , speed    = speedIn body
        , versus   = them
        }
      }

-- | How long this one is allowed to take.
--
-- The settings arrive as a string with more JSON inside it rather than
-- as an object, so they are read twice. A game whose speed cannot be
-- read is taken as a live one, which is what most games are, and is
-- only what the window shows anyway.
speedIn :: Value -> Speed
speedIn body = case said of
  Just word ->
    maybe Live id (listToMaybe [ how | how <- speeds, speedName how == word ])
  Nothing -> Live
 where
  said = do
    inside <- fieldOf "time_control_parameters" body >>= asString
    held   <- either (const Nothing) Just
                     (eitherDecodeStrict (Text.encodeUtf8 inside) :: Either String Value)
    fieldOf "speed" held >>= asString

fieldOf :: Text -> Value -> Maybe Value
fieldOf key = \case
  Object fields -> Aeson.parseMaybe (Aeson..: Key.fromText key) fields
  _             -> Nothing

asWhole :: Value -> Maybe Int
asWhole = \case
  Number n -> toBoundedInteger n
  _        -> Nothing

asString :: Value -> Maybe Text
asString = \case
  String text -> Just text
  _           -> Nothing

asBool :: Value -> Maybe Bool
asBool = \case
  Bool yes -> Just yes
  _        -> Nothing
