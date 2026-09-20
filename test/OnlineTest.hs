-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Games on online-go.com, without online-go.com.
--
-- Both of the things that reach off this machine are arguments, so the
-- whole run is driven here: signing in answers with whoever the test
-- says, and the connection is a mailbox a test pushes events into.
-- What is checked is the order it all happens in, and what is said to
-- the server along the way.
module OnlineTest
  ( tests
  )
where

import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.STM         ( atomically )
import           Control.Monad                  ( void )
import           Data.Aeson                     ( Value(..)
                                                , object
                                                , (.=)
                                                )
import qualified Data.Aeson.Key                as Key
import qualified Data.Aeson.KeyMap             as KeyMap
import qualified Data.Aeson.Types              as Aeson
import           Data.IORef
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Hedgehog
import           Pipes.Concurrent               ( fromInput
                                                , send
                                                , spawn'
                                                , unbounded
                                                )
import           System.Timeout                 ( timeout )

import           Go.Types
import           Stones.Ogs.Account             ( Account(..) )
import           Stones.Ogs.Frame               ( Packet(..) )
import           Stones.Ogs.Socket              ( Socket(..) )
import           Stones.Log                     ( quiet )
import           Stones.Online
import           Stones.Opponent
import           Stones.Setup                   ( Against(..)
                                                , Handicapping(..)
                                                , Setup(..)
                                                , Speed(..)
                                                , Strength(..)
                                                )

-- | The player these are all about.
me :: Account
me = Account { player = 77, username = "hikaru", jwt = "j", chatAuth = "c" }

-- | The same, signed in, which is what the signing in answers with.
signedInAsMe :: Signed
signedInAsMe = Signed { account = me, token = "a token" }

-- | A game to ask for.
wanted :: Setup
wanted = Setup { against  = Online
               , size     = 9
               , human    = Black
               , strength = Strength 10
               , handicap = 0
               , speed    = Live
               , ranked   = False
               , handicapping = Standard
               , versus   = ""
               }

-- | A connection that goes nowhere, and the handles to drive it.
data Fake = Fake
  { connection :: Socket
  , sent       :: IO [Packet]
  , arrive     :: Packet -> IO ()
  , stopped    :: IO Int
  }

fake :: IO Fake
fake = do
  written                 <- newIORef []
  stops                   <- newIORef (0 :: Int)
  (toHear, hearing, seal) <- spawn' unbounded
  pure Fake
    { connection = Socket
      { put   = \packet -> Right () <$ modifyIORef' written (<> [packet])
      , heard = fromInput hearing
      , stop  = modifyIORef' stops (+ 1) >> atomically seal
      }
    , sent       = readIORef written
    , arrive     = void . atomically . send toHear . Right
    , stopped    = readIORef stops
    }

-- | What the server sends when a looking is answered.
paired :: Text -> Int -> Packet
paired name game = Event
  "automatch/start"
  [object ["uuid" .= name, "game_id" .= game]]
  Nothing

-- | And what it says about the game itself, which is what a game waits
-- for before anybody is given one to play.
standing :: Int -> Packet
standing game = Event
  ("game/" <> Text.pack (show game) <> "/gamedata")
  [ object
      [ "width" .= (9 :: Int)
      , "initial_player" .= ("black" :: Text)
      , "black_player_id" .= (77 :: Int)
      , "white_player_id" .= (88 :: Int)
      , "moves" .= ([] :: [Value])
      ]
  ]
  Nothing

-- | The name of the looking that went out, out of what was sent.
lookingIn :: [Packet] -> Maybe Text
lookingIn packets = case
    [ body | Event "automatch/find_match" (body : _) _ <- packets ]
  of
    body : _ -> case body of
      Object _ -> case fieldOf "uuid" body of
        Just (String name) -> Just name
        _                  -> Nothing
      _ -> Nothing
    [] -> Nothing
 where
  fieldOf key = \case
    Object fields -> case
        [ held | (held', held) <- objectPairs fields, held' == key ]
      of
        found : _ -> Just found
        []        -> Nothing
    _ -> Nothing

-- | The names of the events that went out, in order.
namesOf :: [Packet] -> [Text]
namesOf packets = [ name | Event name _ _ <- packets ]

prop_aGameCannotBeAskedForBeforeAnybodyIsSignedIn :: Property
prop_aGameCannotBeAskedForBeforeAnybodyIsSignedIn =
  withTests 1 . property $ do
    -- Nothing is dialled either. A connection opened before there is
    -- anybody to be would be a connection with nothing to say.
    dials <- evalIO (newIORef (0 :: Int))
    asked <- evalIO . withOnlineWith quiet
      Reaching { signIn = pure (Right signedInAsMe)
               , dial   = modifyIORef' dials (+ 1) >> pure (Left "not dialled")
               }
      $ \source -> source.open wanted
    case asked of
      Right _  -> annotate "it found a game" >> failure
      Left why -> why === "Sign in to online-go.com first."
    opened <- evalIO (readIORef dials)
    opened === 0

prop_signingInSaysWhoCameBack :: Property
prop_signingInSaysWhoCameBack = withTests 1 . property $ do
  name <- evalIO . withOnlineWith quiet
    Reaching { signIn = pure (Right signedInAsMe), dial = pure (Left "not dialled") }
    $ \source -> source.enter
  name === Right "hikaru"
  -- And a refusal is passed on as it stands, rather than being turned
  -- into something about this program.
  refused <- evalIO . withOnlineWith quiet
    Reaching { signIn = pure (Left "the browser never came back")
             , dial   = pure (Left "not dialled")
             }
    $ \source -> source.enter
  refused === Left "the browser never came back"

prop_aGameIsSignedInForAndThenLookedFor :: Property
prop_aGameIsSignedInForAndThenLookedFor = withTests 1 . property $ do
  it     <- evalIO fake
  played <- evalIO . withOnlineWith quiet
    Reaching { signIn = pure (Right signedInAsMe), dial = pure (Right it.connection) }
    $ \source -> do
        _ <- source.enter
        -- The looking is answered as soon as it goes out, which is
        -- what the server does when somebody else is already waiting.
        answered <- forkIO $ let
          watch = it.sent >>= \packets -> case lookingIn packets of
            Just name -> do
              it.arrive (paired name 1234)
              it.arrive (standing 1234)
            Nothing -> watch
          in watch
        _ <- pure answered
        timeout 5000000 (source.open wanted)
  case played of
    Nothing            -> annotate "it never came back" >> failure
    Just (Left  why  ) -> annotate (show why) >> failure
    Just (Right them) -> them.name === "online-go.com"
  -- Signed in first, then the looking, then the game joined. Asking
  -- for a game on a connection nobody has signed in on is a connection
  -- the server answers nothing on.
  said <- evalIO it.sent
  namesOf said === ["authenticate", "automatch/find_match", "game/connect"]

prop_namingSomebodyAsksThemRatherThanLookingForAnybody :: Property
prop_namingSomebodyAsksThemRatherThanLookingForAnybody =
  withTests 1 . property $ do
    -- Asking a named player is a call to the website, so with no
    -- network the call fails and nothing goes over the connection.
    -- What matters is which of the two paths was taken: a looking
    -- would have gone out first.
    it    <- evalIO fake
    _     <- evalIO . withOnlineWith quiet
      Reaching { signIn = pure (Right signedInAsMe)
               , dial   = pure (Right it.connection)
               }
      $ \source -> do
          _ <- source.enter
          timeout 20000000 (source.open wanted { versus = "a-bot" })
    said  <- evalIO it.sent
    -- No looking, because nobody was being looked for.
    assert (notElem "automatch/find_match" (namesOf said))
    assert (notElem "automatch/cancel" (namesOf said))

prop_aLookingThatComesToNothingIsCalledOffAndTheConnectionClosed :: Property
prop_aLookingThatComesToNothingIsCalledOffAndTheConnectionClosed =
  withTests 1 . property $ do
    it     <- evalIO fake
    -- Nothing answers, and the connection goes away underneath it,
    -- which is what a server restarting looks like.
    asked  <- evalIO . withOnlineWith quiet
      Reaching { signIn = pure (Right signedInAsMe), dial = pure (Right it.connection) }
      $ \source -> do
          _ <- source.enter
          done <- forkIO (it.connection.stop)
          _ <- pure done
          timeout 5000000 (source.open wanted)
    case asked of
      Just (Left _) -> success
      _             -> annotate "it found a game anyway" >> failure
    -- The looking is called off before the connection closes, because
    -- a connection that goes away without a word is one the server may
    -- go on holding a place at a board for.
    said   <- evalIO it.sent
    namesOf said === ["authenticate", "automatch/find_match", "automatch/cancel"]

prop_everyConnectionOpenedIsClosedWhenTheWindowGoes :: Property
prop_everyConnectionOpenedIsClosedWhenTheWindowGoes =
  withTests 1 . property $ do
    it <- evalIO fake
    _  <- evalIO . withOnlineWith quiet
      Reaching { signIn = pure (Right signedInAsMe), dial = pure (Right it.connection) }
      $ \source -> do
          _ <- source.enter
          timeout 200000 (source.open wanted)
    -- Whatever was opened is closed, so a window that ends leaves
    -- nothing of itself on the server.
    stops <- evalIO it.stopped
    assert (stops >= 1)

prop_patienceIsLongEnoughToBePairedAndShortEnoughToLetGo :: Property
prop_patienceIsLongEnoughToBePairedAndShortEnoughToLetGo =
  withTests 1 . property $ do
    -- Being paired waits for somebody else looking for the same game
    -- at the same time.
    assert (patience >= 60)
    assert (patience <= 60 * 60)

-- | The pairs of a JSON object, which the test above reads a name out
-- of.
objectPairs :: Aeson.Object -> [(Text, Value)]
objectPairs = map (\(key, held) -> (Key.toText key, held)) . KeyMap.toList

tests :: Group
tests = $$(discover)
