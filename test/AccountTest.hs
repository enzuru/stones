-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later


-- | Who a token belongs to, and signing a connection in as them.
--
-- The call that asks is the only part that leaves this machine, and it
-- is not here. What is here is reading its answer and writing the
-- frame that follows.
module AccountTest
  ( tests
  )
where

import           Data.Aeson                     ( object
                                                , (.=)
                                                )
import           Data.IORef
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as Text
import           Hedgehog

import           Stones.Ogs.Account
import           Stones.Ogs.Frame               ( Packet(..) )
import           Stones.Ogs.Socket              ( Socket(..) )

-- | What the call answers with, cut down to the parts that matter.
config :: Text
config =
  "{\"user\":{\"id\":1010740,\"username\":\"someone\",\"ranking\":23},\
  \\"user_jwt\":\"a.b.c\",\"chat_auth\":\"abc123\",\"cdn\":\"https://cdn\"}"

-- | The account that is in it.
someone :: Account
someone = Account { player   = 1010740
                  , username = "someone"
                  , jwt      = "a.b.c"
                  , chatAuth = "abc123"
                  }

prop_whoTheTokenBelongsToIsReadOutOfTheAnswer :: Property
prop_whoTheTokenBelongsToIsReadOutOfTheAnswer = withTests 1 . property $ do
  -- The answer carries a great deal more than this, and the rest of it
  -- is none of this program's business.
  accountFrom (Text.encodeUtf8 config) === Right someone

prop_theTalkingCredentialIsNotPromised :: Property
prop_theTalkingCredentialIsNotPromised = withTests 1 . property $ do
  accountFrom
      (Text.encodeUtf8
        "{\"user\":{\"id\":1,\"username\":\"x\"},\"user_jwt\":\"j\"}"
      )
    === Right
          Account { player = 1, username = "x", jwt = "j", chatAuth = "" }

prop_anAnswerWithoutWhatIsNeededIsRefused :: Property
prop_anAnswerWithoutWhatIsNeededIsRefused = withTests 1 . property $ do
  -- Without the socket's own credential there is no connection to be
  -- had, so an answer missing it is no answer.
  assert (isLeft (accountFrom (Text.encodeUtf8 "{\"user\":{\"id\":1,\"username\":\"x\"}}")))
  assert (isLeft (accountFrom (Text.encodeUtf8 "{\"user_jwt\":\"j\"}")))
  assert (isLeft (accountFrom (Text.encodeUtf8 "not json")))
  assert (isLeft (accountFrom (Text.encodeUtf8 "{}")))

prop_signingInSaysWhoIsSigningIn :: Property
prop_signingInSaysWhoIsSigningIn = withTests 1 . property $ do
  authenticating someone
    === Event
          "authenticate"
          [ object
              [ "auth" .= ("abc123" :: Text)
              , "jwt" .= ("a.b.c" :: Text)
              , "player_id" .= (1010740 :: Int)
              , "username" .= ("someone" :: Text)
              ]
          ]
          Nothing

prop_signingInPutsThatOnTheConnection :: Property
prop_signingInPutsThatOnTheConnection = withTests 1 . property $ do
  (outcome, written) <- evalIO $ do
    sent <- newIORef []
    let connection = Socket
          { put   = \packet -> Right () <$ modifyIORef' sent (<> ([packet] :: [Packet]))
          , heard = pure ()
          , stop  = pure ()
          }
    answer <- signingIn connection someone
    (,) answer <$> readIORef sent
  outcome === Right ()
  written === [authenticating someone]

prop_aClosedConnectionCannotBeSignedInTo :: Property
prop_aClosedConnectionCannotBeSignedInTo = withTests 1 . property $ do
  outcome <- evalIO $ signingIn
    Socket { put   = \_ -> pure (Left "the connection has closed")
           , heard = pure ()
           , stop  = pure ()
           }
    someone
  outcome === Left "the connection has closed"

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

tests :: Group
tests = $$(discover)
