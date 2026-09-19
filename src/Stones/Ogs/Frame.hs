-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | The frames the socket carries, and how to read and write one.
--
-- online-go.com speaks socket.io, which is two protocols stacked. The
-- outer one, engine.io, keeps the connection alive and says what kind
-- of thing a frame is. The inner one, socket.io, carries the named
-- events the program actually cares about. Both of them are written
-- as digits at the front of a text frame, so a whole event looks like
-- this:
--
-- > 42["game/move",{"game_id":1,"move":"dd"}]
--
-- The @4@ says the outer protocol is carrying a message. The @2@ says
-- the inner one is an event. The rest is a JSON array whose first item
-- is the name.
--
-- There is no client for this in the Haskell packages, and what is
-- there is server-side, so this is the whole of it. It is a pair of
-- functions over text, which is why it is tested without a socket.
module Stones.Ogs.Frame
  ( Packet(..)
  , Handshake(..)
  , encode
  , decode
  )
where

import           Data.Aeson                     ( FromJSON(..)
                                                , Value(..)
                                                , eitherDecodeStrict
                                                , withObject
                                                , (.:)
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Char                      ( isDigit )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Text.Lazy                as Lazy.Text
import qualified Data.Text.Lazy.Encoding       as Lazy.Text
import qualified Data.Vector                   as Vector
import           Text.Read                      ( readMaybe )

-- | What the server says when the socket opens.
data Handshake = Handshake
  { sid          :: Text
    -- ^ The name this connection goes by.
  , pingInterval :: Int
    -- ^ How often to send a 'Ping', in milliseconds. In this version
    -- of the protocol the client is the one that pings.
  , pingTimeout  :: Int
    -- ^ How long the server waits for one before giving up, in
    -- milliseconds.
  }
  deriving (Eq, Show)

instance FromJSON Handshake where
  parseJSON = withObject "handshake" $ \fields ->
    Handshake
      <$> fields
      .:  "sid"
      <*> fields
      .:  "pingInterval"
      <*> fields
      .:  "pingTimeout"

-- | One frame.
data Packet
  = Open Handshake
    -- ^ The first frame the server sends.
  | Close
  | Ping
  | Pong
  | Upgrade
  | Noop
  | Connected
    -- ^ The inner protocol is ready to carry events.
  | Disconnected
  | Event Text [Value] (Maybe Int)
    -- ^ A name, its arguments, and the number to answer under if the
    -- sender wants an answer.
  | Ack Int [Value]
    -- ^ An answer to an event that asked for one.
  | Failed Value
    -- ^ The inner protocol refused something.
  deriving (Eq, Show)

-- | Write a frame.
encode :: Packet -> Text
encode = \case
  -- Nothing here ever sends one of these. It is written for the sake
  -- of the other direction being readable.
  Open _            -> "0"
  Close             -> "1"
  Ping              -> "2"
  Pong              -> "3"
  Upgrade           -> "5"
  Noop              -> "6"
  Connected         -> "40"
  Disconnected      -> "41"
  Event name args answering ->
    "42" <> number answering <> asJson (String name : args)
  Ack answering args -> "43" <> Text.pack (show answering) <> asJson args
  -- A refusal carries one value rather than a list of them, which is
  -- what the other direction reads.
  Failed what        -> "44" <> asValue what
 where
  number = maybe "" (Text.pack . show)
  asJson :: [Value] -> Text
  asJson = asValue . Array . Vector.fromList
  asValue :: Value -> Text
  asValue = Lazy.Text.toStrict . Lazy.Text.decodeUtf8 . Aeson.encode

-- | Read a frame.
decode :: Text -> Either Text Packet
decode text = case Text.uncons text of
  Nothing          -> Left "an empty frame"
  Just ('0', rest) -> Open <$> readJson rest
  Just ('1', _   ) -> Right Close
  Just ('2', _   ) -> Right Ping
  Just ('3', _   ) -> Right Pong
  Just ('4', rest) -> message rest
  Just ('5', _   ) -> Right Upgrade
  Just ('6', _   ) -> Right Noop
  Just (other, _) ->
    Left ("a frame of an unknown kind: " <> Text.singleton other)

-- | Read what the inner protocol put inside a message.
message :: Text -> Either Text Packet
message text = case Text.uncons text of
  Nothing          -> Left "a message with nothing in it"
  Just ('0', _   ) -> Right Connected
  Just ('1', _   ) -> Right Disconnected
  Just ('2', rest) -> event (afterNamespace rest)
  Just ('3', rest) -> acknowledgement (afterNamespace rest)
  Just ('4', rest) -> Failed <$> readJson (afterNamespace rest)
  Just (other, _) ->
    Left ("a message of an unknown kind: " <> Text.singleton other)

-- | A frame may name a namespace before anything else. This program
-- uses the one that has no name, and skips the name when there is one
-- rather than refusing the frame.
afterNamespace :: Text -> Text
afterNamespace text
  | Just ('/', _) <- Text.uncons text = Text.drop 1 (Text.dropWhile (/= ',') text)
  | otherwise                         = text

event :: Text -> Either Text Packet
event text = do
  args <- readArray rest
  case args of
    String name : after -> Right (Event name after (readMaybe (Text.unpack digits)))
    _                   -> Left "an event with no name"
  where (digits, rest) = Text.span isDigit text

acknowledgement :: Text -> Either Text Packet
acknowledgement text = case readMaybe (Text.unpack digits) of
  Nothing        -> Left "an answer with no number to answer under"
  Just answering -> Ack answering <$> readArray rest
  where (digits, rest) = Text.span isDigit text

-- | Read what is left of a frame as a JSON array.
readArray :: Text -> Either Text [Value]
readArray text = readJson text >>= \case
  Array items -> Right (Vector.toList items)
  _           -> Left "a frame whose payload is not a list"

-- | Read what is left of a frame as JSON.
readJson :: FromJSON a => Text -> Either Text a
readJson text = case eitherDecodeStrict (Text.encodeUtf8 text) of
  Left  why   -> Left ("a frame this program cannot read: " <> Text.pack why)
  Right value -> Right value
