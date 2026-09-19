-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}

-- | Where online-go.com is, and who this program says it is.
module Stones.Ogs
  ( clientId
  , authorizeUrl
  , tokenUrl
  , socketUrl
  , redirectPorts
  , redirectFor
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

-- | The application this program registered as.
--
-- Not a secret. A public client is one that cannot keep a secret, so
-- it is issued none, and its identifier ships inside it: anybody can
-- read this out of the binary and it costs nothing, because the
-- redirect it is tied to only goes back to the machine it started on.
clientId :: Text
clientId = "BP9wOZuzl54POhMoaA7uJ1duIXzOfyMvLFQB3qOh"

-- | Where a person is sent to say yes.
authorizeUrl :: Text
authorizeUrl = "https://online-go.com/oauth2/authorize/"

-- | Where the code is traded for a token.
tokenUrl :: Text
tokenUrl = "https://online-go.com/oauth2/token/"

-- | The socket everything that happens during a game arrives on.
--
-- @EIO=3@ asks for version 3 of the engine.io framing, which is the
-- one this server speaks, and @transport=websocket@ asks to skip the
-- long-polling it would otherwise start with.
socketUrl :: Text
socketUrl = "wss://online-go.com/socket.io/?transport=websocket&EIO=3"

-- | The ports the browser may be sent back to.
--
-- More than one because a redirect has to match what was registered
-- exactly, so the port is part of it, and a port can be taken by
-- something else. These are the three the application was registered
-- with, and the first free one is used.
redirectPorts :: [Int]
redirectPorts = [41414, 41415, 41416]

-- | The address the browser is sent back to, for a port.
--
-- The literal address rather than @localhost@, which can resolve to
-- ::1 while this program is listening on IPv4.
redirectFor :: Int -> Text
redirectFor port =
  "http://127.0.0.1:" <> Text.pack (show port) <> "/callback"
