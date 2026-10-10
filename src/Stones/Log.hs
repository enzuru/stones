-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE NoFieldSelectors      #-}

-- | Where this program writes down what it did.
--
-- Nothing is written unless somebody asked for it on the command line.
-- What it is for is finding out what online-go.com actually says,
-- which is the one thing no test on this machine can settle: every
-- fake here answers the way this program expects, which is exactly the
-- assumption in question.
module Stones.Log
  ( Log(..)
  , quiet
  , toFile
  , guarding
  , secrets
  , watching
  )
where

import           Data.Aeson                     ( Value(..) )
import qualified Data.Aeson.Key                as Key
import qualified Data.Aeson.KeyMap             as KeyMap
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import           Pipes                          ( (>->) )
import qualified Pipes.Prelude                 as Pipes
import           System.IO                      ( BufferMode(LineBuffering)
                                                , IOMode(AppendMode)
                                                , hSetBuffering
                                                , openFile
                                                )

import           Stones.Ogs.Frame               ( Packet(..)
                                                , encode
                                                )
import           Stones.Ogs.Socket              ( Socket(..) )

-- | Somewhere to write a line.
newtype Log = Log
  { note :: Text -> IO ()
  }

-- | One that writes nothing, which is what a program nobody asked has.
quiet :: Log
quiet = Log { note = \_ -> pure () }

-- | One that appends to a file, a line at a time, with the time on
-- each.
--
-- Line buffered, because the whole point is to read it while the
-- program is still running, and a program still running has not
-- flushed anything it did not have to.
toFile :: FilePath -> IO Log
toFile path = do
  handle <- openFile path AppendMode
  hSetBuffering handle LineBuffering
  pure Log
    { note = \line -> do
      now <- getCurrentTime
      Text.hPutStrLn
        handle
        (Text.pack (formatTime defaultTimeLocale "%H:%M:%S%3Q" now) <> "  " <> line)
    }

-- | The fields that carry something nobody else should have.
--
-- Named rather than guessed at. A field that carries a secret and is
-- not on this list is a mistake to fix here, and a log is a file that
-- ends up pasted into a bug report.
secrets :: [Text]
secrets =
  [ "auth"
  , "chat_auth"
  , "jwt"
  , "access_token"
  , "refresh_token"
  , "player_auth"
  , "token"
  , "password"
  , "client_secret"
  , "code_verifier"
  ]

-- | The same thing with those fields taken out, however deep they are.
guarding :: Value -> Value
guarding = \case
  Object fields -> Object
    (KeyMap.mapWithKey
      (\key held ->
        if Key.toText key `elem` secrets then String "(hidden)" else guarding held
      )
      fields
    )
  Array items -> Array (fmap guarding items)
  other       -> other

-- | The same connection, with everything that goes over it written
-- down.
--
-- A decorator rather than something inside the socket, so that the
-- socket knows nothing about logs and a test can put one around a
-- connection that goes nowhere.
watching :: Log -> Socket -> Socket
watching log' connection = Socket
  { put   = \packet -> do
    log'.note ("--> " <> shown packet)
    answer <- connection.put packet
    case answer of
      Left why -> log'.note ("--> refused: " <> why)
      Right () -> pure ()
    pure answer
  , heard = connection.heard >-> Pipes.chain noting
  , stop  = log'.note "--- closing the connection" >> connection.stop
  }
 where
  noting = \case
    Left  why    -> log'.note ("<-- broken: " <> why)
    Right packet -> log'.note ("<-- " <> shown packet)

-- | A packet as a line of a log: what it is, with the secrets in it
-- taken out.
shown :: Packet -> Text
shown packet = encode (case packet of
  Event name arguments ack -> Event name (map guarding arguments) ack
  Ack   ack  arguments     -> Ack ack (map guarding arguments)
  Failed held              -> Failed (guarding held)
  other                    -> other)
