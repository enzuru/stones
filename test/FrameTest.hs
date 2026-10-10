-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later


-- | Reading and writing the frames the online-go.com socket carries.
--
-- A frame is text, so all of this runs without a socket, a server or a
-- network. The frames quoted below are the shapes the real server
-- sends.
module FrameTest
  ( tests
  )
where

import           Data.Aeson                     ( Value(..)
                                                , object
                                                , (.=)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Stones.Ogs.Frame

-- | A JSON value of the kinds this protocol carries. Whole numbers
-- only, because a round trip through a decimal point is a different
-- test and not this one.
value :: Gen Value
value = Gen.choice
  [ pure Null
  , Bool <$> Gen.bool
  , Number . fromIntegral <$> Gen.int (Range.linearFrom 0 (-1000) 1000)
  , String <$> Gen.text (Range.linear 0 8) Gen.alphaNum
  , pure (object ["game_id" .= (7 :: Int), "move" .= ("dd" :: Text)])
  ]

-- | A name of the shape this server uses.
name :: Gen Text
name = Gen.element
  (["game/move", "game/connect", "game/chat", "chat/join", "net/ping"] :: [Text])

-- | The frames this program sends. 'Open' is not among them, because
-- only a server sends one.
sendable :: Gen Packet
sendable = Gen.choice
  [ pure Ping
  , pure Pong
  , pure Connected
  , pure Disconnected
  , Event
  <$> name
  <*> Gen.list (Range.linear 0 3) value
  <*> Gen.maybe (Gen.int (Range.linear 0 9999))
  , Ack <$> Gen.int (Range.linear 0 9999) <*> Gen.list (Range.linear 0 3) value
  , Failed <$> value
  ]

prop_whatIsWrittenReadsBackAsWhatItWas :: Property
prop_whatIsWrittenReadsBackAsWhatItWas = property $ do
  packet <- forAll sendable
  decode (encode packet) === Right packet

prop_anEventIsTheDigitsFourTwoAndThenAList :: Property
prop_anEventIsTheDigitsFourTwoAndThenAList = withTests 1 . property $ do
  encode (Event "game/move" [] Nothing) === "42[\"game/move\"]"
  encode (Event "net/ping" [Number 1] Nothing) === "42[\"net/ping\",1]"
  -- The number in front of the list is the one an answer comes back
  -- under.
  encode (Event "game/move" [] (Just 17)) === "4217[\"game/move\"]"
  encode Ping === "2"
  encode Pong === "3"
  encode Connected === "40"

prop_theOpeningFrameSaysHowOftenToPing :: Property
prop_theOpeningFrameSaysHowOftenToPing = withTests 1 . property $ do
  -- This version of the protocol has the client doing the pinging, so
  -- the interval is the one number in here this program has to act on.
  decode
      "0{\"sid\":\"lv_VI97HAXpY6yYWAAAC\",\"upgrades\":[],\"pingInterval\":25000,\"pingTimeout\":60000}"
    === Right
          (Open Handshake { sid          = "lv_VI97HAXpY6yYWAAAC"
                          , pingInterval = 25000
                          , pingTimeout  = 60000
                          }
          )

prop_theFramesTheServerSendsAreRead :: Property
prop_theFramesTheServerSendsAreRead = withTests 1 . property $ do
  decode "2" === Right Ping
  decode "3" === Right Pong
  decode "1" === Right Close
  decode "5" === Right Upgrade
  decode "6" === Right Noop
  decode "40" === Right Connected
  decode "41" === Right Disconnected

prop_aMoveArrivesAsAnEventWithItsGameInTheName :: Property
prop_aMoveArrivesAsAnEventWithItsGameInTheName =
  withTests 1 . property $ do
    decode "42[\"game/1234/move\",{\"move_number\":1,\"move\":[3,3,0]}]"
      === Right
            (Event
              "game/1234/move"
              [ object
                  [ "move_number" .= (1 :: Int)
                  , "move" .= ([3, 3, 0] :: [Int])
                  ]
              ]
              Nothing
            )

prop_anAnswerCarriesTheNumberItAnswers :: Property
prop_anAnswerCarriesTheNumberItAnswers = withTests 1 . property $ do
  decode "43123[{\"ok\":true}]"
    === Right (Ack 123 [object ["ok" .= True]])
  -- An answer with no number is not an answer.
  assert (isLeft (decode "43[{}]"))

prop_aNameForTheDefaultNamespaceIsSkipped :: Property
prop_aNameForTheDefaultNamespaceIsSkipped = withTests 1 . property $ do
  -- This program uses the namespace that has no name, and reads past
  -- one rather than refusing the frame.
  decode "42/chat,[\"hello\"]" === Right (Event "hello" [] Nothing)
  decode "42/chat,7[\"hello\"]" === Right (Event "hello" [] (Just 7))

prop_aFrameThatMakesNoSenseIsRefused :: Property
prop_aFrameThatMakesNoSenseIsRefused = withTests 1 . property $ do
  assert (isLeft (decode ""))
  assert (isLeft (decode "9"))
  assert (isLeft (decode "4"))
  assert (isLeft (decode "49"))
  assert (isLeft (decode "42not json at all"))
  -- A list is what an event is, and a name is what it starts with.
  assert (isLeft (decode "42{\"name\":\"x\"}"))
  assert (isLeft (decode "42[]"))
  assert (isLeft (decode "42[7]"))
  -- And an opening frame this program cannot read is refused rather
  -- than guessed at.
  assert (isLeft (decode "0{\"sid\":\"x\"}"))

prop_everyRefusalSaysSomething :: Property
prop_everyRefusalSaysSomething = property $ do
  frame <- forAll (Gen.text (Range.linear 0 12) Gen.ascii)
  case decode frame of
    Right _  -> success
    Left why -> assert (not (Text.null why))

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

tests :: Group
tests = $$(discover)
