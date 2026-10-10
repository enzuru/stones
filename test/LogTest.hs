-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later


-- | What this program writes down, and what it does not.
--
-- A log is a file that ends up pasted into a bug report, and the
-- connection signs in with a token. So the one thing worth a property
-- of its own is that no secret reaches the file, however deep in a
-- payload it was.
module LogTest
  ( tests
  )
where

import           Control.Concurrent.STM         ( atomically )
import           Control.Monad                  ( void )
import           Data.Aeson                     ( Value(..)
                                                , object
                                                , (.=)
                                                )
import qualified Data.Aeson.Key                as Key
import           Data.IORef
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Pipes.Concurrent               ( fromInput
                                                , send
                                                , spawn'
                                                , unbounded
                                                )
import           Pipes                          ( (>->) )
import qualified Pipes.Prelude                 as Pipes

import           Stones.Log
import           Stones.Ogs.Account             ( Account(..)
                                                , authenticating
                                                )
import           Stones.Ogs.Frame               ( Packet(..) )
import           Stones.Ogs.Socket              ( Socket(..) )

-- | A log that keeps its lines where a test can read them.
kept :: IO (Log, IO [Text])
kept = do
  lines' <- newIORef []
  pure (Log { note = \line -> modifyIORef' lines' (<> [line]) }, readIORef lines')

-- | The player a connection signs in as.
me :: Account
me = Account { player   = 77
             , username = "hikaru"
             , jwt      = "a-secret-token"
             , chatAuth = "another-secret"
             }

prop_nothingSecretReachesTheFile :: Property
prop_nothingSecretReachesTheFile = withTests 1 . property $ do
  -- Signing in is the one frame that carries them, and it is the first
  -- thing that goes over every connection.
  (log', lines') <- evalIO kept
  it             <- evalIO (fake log')
  _              <- evalIO (it.put (authenticating me))
  written        <- evalIO lines'
  let whole = Text.unlines written
  -- Who it is is worth writing down. What proves it is not.
  assert (Text.isInfixOf "hikaru" whole)
  assert (Text.isInfixOf "77" whole)
  assert (not (Text.isInfixOf "a-secret-token" whole))
  assert (not (Text.isInfixOf "another-secret" whole))
  assert (Text.isInfixOf "(hidden)" whole)

prop_aSecretIsHiddenHoweverDeepItIs :: Property
prop_aSecretIsHiddenHoweverDeepItIs = property $ do
  hidden <- forAll (Gen.element secrets)
  depth  <- forAll (Gen.int (Range.linear 0 4))
  let buried 0 = object
        [ Key.fromText hidden .= ("nobody-should-see-this" :: Text)
        , "keep" .= ("this one is fine" :: Text)
        ]
      buried n = object ["under" .= ([buried (n - 1 :: Int)] :: [Value])]
      shown'   = Text.pack (show (guarding (buried depth)))
  -- However far down it was put, and whatever it was under.
  assert (not (Text.isInfixOf "nobody-should-see-this" shown'))
  assert (Text.isInfixOf "(hidden)" shown')
  -- And the field beside it is untouched, because a log that hid
  -- everything would say nothing.
  assert (Text.isInfixOf "this one is fine" shown')

prop_whatIsNotASecretIsLeftAlone :: Property
prop_whatIsNotASecretIsLeftAlone = property $ do
  -- A log that hid everything would say nothing.
  held <- forAll (Gen.text (Range.linear 1 20) Gen.alphaNum)
  guarding (object ["game_id" .= (1234 :: Int), "move" .= held])
    === object ["game_id" .= (1234 :: Int), "move" .= held]
  guarding (String held) === String held

prop_bothDirectionsAreWrittenDown :: Property
prop_bothDirectionsAreWrittenDown = withTests 1 . property $ do
  (log', lines') <- evalIO kept
  written        <- evalIO $ do
    it <- fake log'
    _  <- it.put (Event "game/connect" [object ["game_id" .= (1234 :: Int)]] Nothing)
    -- One arrives, and is read off the stream the way a game reads it.
    _  <- Pipes.toListM (it.heard >-> Pipes.take 1)
    it.stop
    lines'
  let whole = Text.unlines written
  -- Which way each one went, because half the point is the order.
  assert (Text.isInfixOf "--> " whole)
  assert (Text.isInfixOf "<-- " whole)
  assert (Text.isInfixOf "game/connect" whole)
  assert (Text.isInfixOf "game/1234/move" whole)
  assert (Text.isInfixOf "closing the connection" whole)

prop_aQuietLogWritesNothingAndTakesNothingAway :: Property
prop_aQuietLogWritesNothingAndTakesNothingAway =
  withTests 1 . property $ do
    -- A program nobody asked has one of these, and the connection has
    -- to work exactly as it did.
    answer <- evalIO $ do
      it <- fake quiet
      it.put (Event "game/connect" [] Nothing)
    answer === Right ()

-- | A connection that goes nowhere, with one event already waiting on
-- it, wrapped in the log under test.
fake :: Log -> IO Socket
fake log' = do
  (toHear, hearing, seal) <- spawn' unbounded
  void
    (atomically
      (send
        toHear
        (Right (Event "game/1234/move" [object ["move_number" .= (1 :: Int)]] Nothing))
      )
    )
  pure
    (watching
      log'
      Socket { put   = \_ -> pure (Right ())
             , heard = fromInput hearing
             , stop  = atomically seal
             }
    )

tests :: Group
tests = $$(discover)
