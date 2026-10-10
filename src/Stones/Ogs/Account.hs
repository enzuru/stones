-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE NoFieldSelectors      #-}

-- | Who a token belongs to, and signing a connection in as them.
--
-- The token that comes back from signing in opens the calls, and the
-- socket will not take it: it wants one of its own, which is handed
-- out beside everything else the browser would need. So there is one
-- call between having a token and having a connection anybody is
-- listening to.
--
-- Reading the answer and writing the frame are both functions of
-- their arguments. The call itself is the only part that leaves this
-- machine.
module Stones.Ogs.Account
  ( Account(..)
  , accountFrom
  , authenticating
  , signingIn
  , whoAmI
  )
where

import           Control.Exception              ( SomeException
                                                , try
                                                )
import           Data.Aeson                     ( FromJSON(..)
                                                , eitherDecodeStrict
                                                , object
                                                , withObject
                                                , (.!=)
                                                , (.:)
                                                , (.:?)
                                                , (.=)
                                                )
import           Data.Bifunctor                 ( first )
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Lazy          as Lazy
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Network.HTTP.Client           as Http
import qualified Network.HTTP.Client.TLS       as Http

import           Stones.Ogs.Frame               ( Packet(..) )
import           Stones.Ogs.Socket              ( Socket(..) )

-- | Who somebody is on online-go.com.
data Account = Account
  { player   :: Int
    -- ^ The number the server knows them by, which every game they
    -- are in is about.
  , username :: Text
  , jwt      :: Text
    -- ^ What the socket takes instead of the token.
  , chatAuth :: Text
    -- ^ What it takes for the talking.
  }
  deriving (Eq, Show)

instance FromJSON Account where
  parseJSON = withObject "config" $ \fields -> do
    who <- fields .: "user"
    Account
      <$> who
      .:  "id"
      <*> who
      .:  "username"
      <*> fields
      .:  "user_jwt"
      <*> (fields .:? "chat_auth" .!= "")

-- | Read what the call answered.
accountFrom :: ByteString.ByteString -> Either Text Account
accountFrom = first (("online-go.com answered with " <>) . Text.pack)
  . eitherDecodeStrict

-- | The frame that signs a connection in.
--
-- Everything the server is told here it already knows. It is told
-- anyway, because that is the shape of the event.
authenticating :: Account -> Packet
authenticating account = Event
  "authenticate"
  [ object
      [ "auth" .= account.chatAuth
      , "jwt" .= account.jwt
      , "player_id" .= account.player
      , "username" .= account.username
      ]
  ]
  Nothing

-- | Sign a connection in.
signingIn :: Socket -> Account -> IO (Either Text ())
signingIn connection = connection.put . authenticating

-- | Ask online-go.com who a token belongs to.
whoAmI :: Text -> IO (Either Text Account)
whoAmI token = do
  manager <- Http.newManager Http.tlsManagerSettings
  asked   <- try (Http.parseRequest "https://online-go.com/api/v1/ui/config")
  case asked of
    Left (problem :: SomeException) -> pure (Left (Text.pack (show problem)))
    Right blank                     -> do
      let request = blank
            { Http.requestHeaders =
              [("Authorization", "Bearer " <> Text.encodeUtf8 token)]
            }
      answered <- try (Http.httpLbs request manager)
      pure $ case answered of
        Left (problem :: SomeException) ->
          Left ("Could not reach online-go.com: " <> Text.pack (show problem))
        Right answer -> accountFrom (Lazy.toStrict (Http.responseBody answer))
