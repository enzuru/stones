-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Challenging somebody in particular.
--
-- Looking for a game pairs this program with whoever is waiting, and
-- whoever is waiting is usually a person. A program that has never run
-- against the real server before has no business being the other half
-- of somebody's game, so the first games it plays should be against a
-- machine that does not mind. That is what this is for: naming an
-- opponent rather than taking the next one in the queue.
--
-- Building the request and reading the answer are functions of their
-- arguments. The two calls are the only parts that leave this machine.
module Stones.Ogs.Challenge
  ( Started(..)
  , asking
  , startedFrom
  , playerFrom
  , challenging
  , findPlayer
  )
where

import           Control.Exception              ( SomeException
                                                , try
                                                )
import           Data.Aeson                     ( FromJSON(..)
                                                , Value(..)
                                                , eitherDecodeStrict
                                                , object
                                                , withObject
                                                , (.:)
                                                , (.=)
                                                )
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Types              as Aeson
import           Data.Bifunctor                 ( first )
import           Data.Maybe                     ( listToMaybe )
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Lazy          as Lazy
import           Data.Scientific                ( toBoundedInteger )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Vector                   as Vector
import qualified Network.HTTP.Client           as Http
import qualified Network.HTTP.Client.TLS       as Http

import           Go.Types
import           Stones.Setup                   ( Handicapping(..)
                                                , Setup(..)
                                                , Speed(..)
                                                , speedName
                                                )

-- | What a challenge that was taken up comes back as.
data Started = Started
  { challenge :: Int
  , game      :: Int
    -- ^ The number to join, which is the whole point of the exercise.
  }
  deriving (Eq, Show)

instance FromJSON Started where
  parseJSON = withObject "challenge" $ \fields ->
    Started <$> fields .: "challenge" <*> fields .: "game"

-- | What to ask for.
--
-- Two of these are decisions rather than settings, and both are about
-- being a good guest.
--
-- Unranked, always. A rank belongs to the person who earned it, and
-- these are games played by a program to find out whether it works.
--
-- The clock comes from the speed that was asked for. It also has to be
-- one the other side will take: a program asked for a game usually has
-- a limit on how long a game it will sit through, and a challenge over
-- that limit is refused rather than played, so these sit inside the
-- ranges the ones on online-go.com publish.
asking :: Setup -> Value
asking setup = object
  [ "challenger_color" .= colorName setup.human
  , "min_ranking" .= (-1000 :: Int)
  , "max_ranking" .= (1000 :: Int)
  , "game" .= object
    [ "name" .= ("Stones" :: Text)
    , "rules" .= ("japanese" :: Text)
    , "ranked" .= setup.ranked
    , "width" .= setup.size
    , "height" .= setup.size
    , "handicap" .= stones
    , "komi_auto" .= ("automatic" :: Text)
    , "disable_analysis" .= False
    , "pause_on_weekends" .= False
    , "private" .= False
    , "time_control" .= ("byoyomi" :: Text)
    , "time_control_parameters" .= object
      [ "system" .= ("byoyomi" :: Text)
      , "speed" .= speedName setup.speed
      , "main_time" .= think
      , "period_time" .= each
      , "periods" .= (5 :: Int)
      ]
    ]
  ]
  where
  (think, each) = clockFor setup.speed
  -- A challenge to somebody in particular carries a number, and the
  -- nearest a number comes to asking for a handicap is minus one,
  -- which tells the server to work one out from the two ranks. That is
  -- also what standard means, so only matchmaking can tell the two
  -- apart: asking one player is asking them, and they either take it
  -- or they do not.
  stones = case setup.handicapping of
    Disabled -> 0 :: Int
    _        -> -1

-- | The time to think in and the length of one short period after it,
-- for a game of this speed.
--
-- Inside the ranges the programs on online-go.com publish for each
-- speed, because a challenge outside one of those is refused rather
-- than played. A day each way for a daily game, which is what makes it
-- a daily game.
clockFor :: Speed -> (Int, Int)
clockFor = \case
  Blitz          -> (30, 10)
  Rapid          -> (120, 20)
  Live           -> (600, 60)
  Correspondence -> (3 * 86400, 86400)

-- | The word the server uses for a colour.
colorName :: Color -> Text
colorName Black = "black"
colorName White = "white"

-- | Read what the server said about a challenge.
startedFrom :: ByteString.ByteString -> Either Text Started
startedFrom = first (("online-go.com answered with " <>) . Text.pack)
  . eitherDecodeStrict

-- | The first player in an answer to a search, if there is one.
playerFrom :: ByteString.ByteString -> Either Text Int
playerFrom body = do
  value <- first (("online-go.com answered with " <>) . Text.pack)
                 (eitherDecodeStrict body)
  case found value of
    Just who -> Right who
    Nothing  -> Left "online-go.com knows nobody by that name"
 where
  found value = do
    results <- field "results" value
    first'  <- case results of
      Array items -> listToMaybe (Vector.toList items)
      _           -> Nothing
    field "id" first' >>= \case
      Number n -> toBoundedInteger n
      _        -> Nothing
  field key = \case
    Object fields -> Aeson.parseMaybe (Aeson..: key) fields
    _             -> Nothing

-- * The calls
--------------

-- | Challenge somebody, and answer with the game it made.
challenging :: Text -> Int -> Setup -> IO (Either Text Started)
challenging token who setup = calling
  token
  ("https://online-go.com/api/v1/players/" <> Text.pack (show who) <> "/challenge")
  (Just (asking setup))
  startedFrom

-- | The number online-go.com knows somebody by, given their name.
findPlayer :: Text -> Text -> IO (Either Text Int)
findPlayer token who = calling
  token
  ("https://online-go.com/api/v1/players/?username=" <> who)
  Nothing
  playerFrom

-- | One call, with the token on it, and what to make of the answer.
calling
  :: Text
  -> Text
  -> Maybe Value
  -> (ByteString.ByteString -> Either Text a)
  -> IO (Either Text a)
calling token address body read' = do
  manager <- Http.newManager Http.tlsManagerSettings
  asked   <- try (Http.parseRequest (Text.unpack address))
  case asked of
    Left (problem :: SomeException) -> pure (Left (Text.pack (show problem)))
    Right blank                     -> do
      let request = case body of
            Nothing -> withToken blank
            Just it -> (withToken blank) { Http.method      = "POST"
                                         , Http.requestBody = Http.RequestBodyLBS
                                           (Aeson.encode it)
                                         }
      answered <- try (Http.httpLbs request manager)
      pure $ case answered of
        Left (problem :: SomeException) ->
          Left ("Could not reach online-go.com: " <> Text.pack (show problem))
        Right answer -> read' (Lazy.toStrict (Http.responseBody answer))
 where
  withToken request = request
    { Http.requestHeaders = [ ("Authorization", "Bearer " <> Text.encodeUtf8 token)
                            , ("Content-Type" , "application/json")
                            ]
    }
