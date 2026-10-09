-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | A game on online-go.com, as an opponent like any other.
--
-- A connection is three actions, so the one here goes nowhere: it
-- writes down what it is given and says whatever a test pushes into
-- it. That leaves the joining itself to be checked, without a socket
-- or a server.
module OgsOpponentTest
  ( tests
  )
where

import           Control.Concurrent.STM         ( atomically )
import           Control.Monad                  ( void )
import           Data.Aeson                     ( Value
                                                , object
                                                , toJSON
                                                , (.=)
                                                )
import           Data.IORef
import           Data.Text                      ( Text )
import           Hedgehog
import           Pipes                          ( (>->) )
import           Pipes.Concurrent               ( fromInput
                                                , send
                                                , spawn'
                                                , unbounded
                                                )
import qualified Pipes.Prelude                 as Pipes
import           System.Timeout                 ( timeout )

import           Go.Types
import           Stones.Ogs.Frame               ( Packet(..) )
import           Stones.Log                     ( quiet )
import           Stones.Ogs.Socket              ( Socket(..) )
import           Stones.Opponent
import           Stones.Opponent.Ogs

-- | A connection that goes nowhere.
data Fake = Fake
  { connection :: Socket
  , sent       :: IO [Packet]
  , arrive     :: Either Text Packet -> IO ()
  , finish     :: IO ()
  }

-- | One of those, optionally refusing everything it is given.
fake :: Bool -> IO Fake
fake taking = do
  written                 <- newIORef []
  (toHear, hearing, seal) <- spawn' unbounded
  pure Fake
    { connection = Socket
      { put   = \packet -> if taking
                  then Right () <$ modifyIORef' written (<> [packet])
                  else pure (Left "the connection has closed")
      , heard = fromInput hearing
      , stop  = pure ()
      }
    , sent       = readIORef written
    , arrive     = void . atomically . send toHear
    , finish     = atomically seal
    }

-- | The game all of these are about.
game :: Int
game = 1234

-- | What the server says about a fresh even game when it is joined.
evenGame :: Packet
evenGame = Event
  "game/1234/gamedata"
  [ object
      [ "width" .= (9 :: Int)
      , "initial_player" .= ("black" :: Text)
      , "black_player_id" .= (77 :: Int)
      , "white_player_id" .= (88 :: Int)
      , "moves" .= ([] :: [Value])
      ]
  ]
  Nothing

-- | What the server sent when a bot was asked for a board it does not
-- play on, word for word.
refusal :: Packet
refusal = Event
  "notification"
  [ object
      [ "type" .= ("gameOfferRejected" :: Text)
      , "game_id" .= game
      , "message"
        .= ("This bot only plays on these board sizes: 9x9, 13x13, 19x19." :: Text)
      , "rejection_details"
        .= object ["rejection_code" .= ("board_size_not_allowed" :: Text)]
      ]
  ]
  Nothing

-- | Join it, on a connection that takes what it is given, with the
-- server already saying where the game stands.
--
-- It has to be waiting there before the joining starts, because
-- nothing is handed back until it arrives.
joinedWith :: Packet -> IO (Fake, Opponent)
joinedWith standing = do
  it <- fake True
  it.arrive (Right standing)
  answer <- joining quiet it.connection 77 game
  case answer of
    Left  why  -> error (show why)
    Right them -> pure (it, them)

-- | The same, in a game that has just started.
joined :: IO (Fake, Opponent)
joined = joinedWith evenGame

-- | A move as it arrives from the server.
moveEvent :: Int -> Int -> Int -> Packet
moveEvent number x y = Event
  ("game/" <> "1234" <> "/move")
  [ object
      [ "game_id" .= game
      , "move_number" .= number
      , "move" .= toJSON [x, y, 0 :: Int]
      ]
  ]
  Nothing

-- | Read this many things off what a game hears.
heardFrom :: Opponent -> Int -> IO (Maybe [Heard])
heardFrom them count =
  timeout 3000000 (Pipes.toListM (them.hear >-> Pipes.take count))

-- | The same, past the one that says where the game started.
--
-- Where it stands is read off the stream before the game is handed
-- back, and put at the front of what the game hears, so it is always
-- the first thing there.
heardAfter :: Opponent -> Int -> IO (Maybe [Heard])
heardAfter them count = fmap (drop 1) <$> heardFrom them (count + 1)

-- | The payload of an event that was sent.
bodyOf :: Packet -> Maybe Value
bodyOf = \case
  Event _ (body : _) _ -> Just body
  _                    -> Nothing

-- | The name of an event that was sent.
namesOf :: [Packet] -> [Text]
namesOf packets = [ name | Event name _ _ <- packets ]

prop_aGameIsNamedAfterWhoItIsAgainst :: Property
prop_aGameIsNamedAfterWhoItIsAgainst = withTests 1 . property $ do
  -- The name goes on the tab, which is the one thing that tells two
  -- games apart at a glance.
  let named = Event
        "game/1234/gamedata"
        [ object
            [ "width" .= (9 :: Int)
            , "initial_player" .= ("black" :: Text)
            , "black_player_id" .= (77 :: Int)
            , "white_player_id" .= (88 :: Int)
            , "moves" .= ([] :: [Value])
            , "players" .= object
              [ "black" .= object ["username" .= ("lenzuru" :: Text)]
              , "white" .= object ["username" .= ("amybot-beginner" :: Text)]
              ]
            ]
        ]
        Nothing
  (_, them) <- evalIO (joinedWith named)
  them.name === "amybot-beginner"
  -- A game that says nothing about who is in it falls back to where it
  -- is being played, which is better than an empty tab.
  (_, nameless) <- evalIO joined
  nameless.name === "online-go.com"

prop_joiningSaysSoOnTheConnection :: Property
prop_joiningSaysSoOnTheConnection = withTests 1 . property $ do
  (it, _) <- evalIO joined
  packets <- evalIO it.sent
  namesOf packets === ["game/connect"]
  -- Chat is asked for at the same time, because a game is the place
  -- the two players talk.
  map bodyOf packets === [Just (object ["game_id" .= game, "chat" .= True])]

prop_leavingSaysSoAndLeavesTheConnectionUp :: Property
prop_leavingSaysSoAndLeavesTheConnectionUp = withTests 1 . property $ do
  -- The next game is on the same connection, so this one does not get
  -- to close it.
  (it, them) <- evalIO joined
  evalIO them.leave
  packets <- evalIO it.sent
  namesOf packets === ["game/connect", "game/disconnect"]

prop_aMoveToldGoesOutAsAnEvent :: Property
prop_aMoveToldGoesOutAsAnEvent = withTests 1 . property $ do
  (it, them) <- evalIO joined
  said       <- evalIO (them.tell (Moved (Play (Coord 3 3))))
  said === Right ()
  packets <- evalIO it.sent
  namesOf packets === ["game/connect", "game/move"]
  map bodyOf (drop 1 packets)
    === [Just (object ["game_id" .= game, "move" .= ("dd" :: Text)])]

prop_whatHasNobodyToSayItToCountsAsSaid :: Property
prop_whatHasNobodyToSayItToCountsAsSaid = withTests 1 . property $ do
  -- The server keeps the rules, so it needs no telling whose turn it
  -- is, and a game that told it anyway would be refused.
  (it, them) <- evalIO joined
  asked      <- evalIO (them.tell Asked)
  counted    <- evalIO (them.tell Counting)
  asked === Right ()
  counted === Right ()
  packets <- evalIO it.sent
  namesOf packets === ["game/connect"]

prop_aConnectionThatWillNotTakeItIsARefusal :: Property
prop_aConnectionThatWillNotTakeItIsARefusal = withTests 1 . property $ do
  it     <- evalIO (fake False)
  answer <- evalIO (joining quiet it.connection 77 game)
  case answer of
    Left why -> why === "the connection has closed"
    Right _  -> annotate "it joined a game on a closed connection" >> failure

prop_aMoveThatArrivesComesBackAsOne :: Property
prop_aMoveThatArrivesComesBackAsOne = withTests 1 . property $ do
  (it, them) <- evalIO joined
  -- Numbered the way the server numbers them, which is a count: the
  -- first move of the game is move number one.
  evalIO (it.arrive (Right (moveEvent 1 3 3)))
  evalIO (it.arrive (Right (moveEvent 2 15 3)))
  arrived <- evalIO (heardAfter them 2)
  arrived
    === Just
          [ Played Black (Play (Coord 3 3))
          , Played White (Play (Coord 15 3))
          ]

prop_whatIsNotAboutThisGameIsNotHeard :: Property
prop_whatIsNotAboutThisGameIsNotHeard = withTests 1 . property $ do
  -- One connection carries every game at once.
  (it, them) <- evalIO joined
  evalIO
    (it.arrive
      (Right
        (Event
          "game/9999/move"
          [ object
              [ "move_number" .= (0 :: Int)
              , "move" .= toJSON [3, 3, 0 :: Int]
              ]
          ]
          Nothing
        )
      )
    )
  -- And the frames the connection needs for itself are not events at
  -- all.
  evalIO (it.arrive (Right Pong))
  evalIO (it.arrive (Right (moveEvent 1 3 3)))
  arrived <- evalIO (heardAfter them 1)
  arrived === Just [Played Black (Play (Coord 3 3))]

prop_theConnectionGoingEndsWhatIsHeard :: Property
prop_theConnectionGoingEndsWhatIsHeard = withTests 1 . property $ do
  -- A game learns its opponent has gone from the stream ending, and
  -- the last thing on it is why.
  (it, them) <- evalIO joined
  evalIO (it.arrive (Left "the connection closed"))
  evalIO it.finish
  arrived <- evalIO (timeout 3000000 (Pipes.toListM them.hear))
  fmap (drop 1) arrived === Just [Lost "the connection closed"]

prop_theCountOfMovesFollowsTheBoard :: Property
prop_theCountOfMovesFollowsTheBoard = withTests 1 . property $ do
  -- Anything said about a game carries where the game was when it was
  -- said, so the count has to follow every move, this program's own
  -- included, because the server says them all.
  (it, them) <- evalIO joined
  -- Numbered the way the server numbers them, which is a count: the
  -- first move of the game is move number one.
  evalIO (it.arrive (Right (moveEvent 1 3 3)))
  evalIO (it.arrive (Right (moveEvent 2 15 3)))
  _ <- evalIO (heardAfter them 2)
  _ <- evalIO (them.tell (Said "good game"))
  packets <- evalIO it.sent
  map bodyOf (drop 1 packets)
    === [ Just
            (object
              [ "game_id" .= game
              , "type" .= ("main" :: Text)
              , "move_number" .= (2 :: Int)
              , "body" .= ("good game" :: Text)
              ]
            )
        ]

-- | What the server says about a handicap game when it is joined:
-- Black's stones are down, so White moves first, and fifty moves have
-- been made since.
handicapGame :: Packet
handicapGame = Event
  "game/1234/gamedata"
  [ object
      [ "width" .= (9 :: Int)
      , "initial_player" .= ("white" :: Text)
      , "initial_state" .= object
        ["black" .= ("ccgg" :: Text), "white" .= ("" :: Text)]
      , "black_player_id" .= (88 :: Int)
      , "white_player_id" .= (77 :: Int)
      , "moves" .= ([] :: [Value])
      ]
  ]
  Nothing

prop_aHandicapGameGetsItsColoursTheRightWayRound :: Property
prop_aHandicapGameGetsItsColoursTheRightWayRound =
  withTests 1 . property $ do
    -- Black's stones are down before anybody has moved, so the first
    -- move is White's. Reading the count without that would have every
    -- colour in the game the wrong way round.
    (it, them) <- evalIO (joinedWith handicapGame)
    evalIO (it.arrive (Right (moveEvent 1 3 3)))
    arrived <- evalIO (heardFrom them 2)
    arrived
      === Just
            [ Began Opening { width  = 9
                            , stones = [(Black, Coord 2 2), (Black, Coord 6 6)]
                            , opens  = White
                            , yours  = White
                            , dead   = Nothing
                            }
            , Played White (Play (Coord 3 3))
            ]

prop_howFarAlongTheGameIsComesFromTheServerToo :: Property
prop_howFarAlongTheGameIsComesFromTheServerToo =
  withTests 1 . property $ do
    -- A game that was left and come back to is already somewhere, and
    -- anything said about it carries where.
    let sofar = Event
          "game/1234/gamedata"
          [ object
              [ "width" .= (9 :: Int)
              , "initial_player" .= ("black" :: Text)
              , "black_player_id" .= (77 :: Int)
              , "white_player_id" .= (88 :: Int)
              , "moves" .= [toJSON [3, 3, 0 :: Int], toJSON [5, 5, 0 :: Int]]
              ]
          ]
          Nothing
    (it, them) <- evalIO (joinedWith sofar)
    _          <- evalIO (them.tell (Said "hello"))
    packets    <- evalIO it.sent
    map bodyOf (drop 1 packets)
      === [ Just
              (object
                [ "game_id" .= game
                , "type" .= ("main" :: Text)
                , "move_number" .= (2 :: Int)
                , "body" .= ("hello" :: Text)
                ]
              )
          ]

prop_aGameTheOtherSideWillNotPlaySaysWhy :: Property
prop_aGameTheOtherSideWillNotPlaySaysWhy = withTests 1 . property $ do
  -- Word for word what online-go.com sent when a bot was asked for a
  -- board it does not play on. It arrives as a notice to the player
  -- rather than as anything about the game, because the game is now
  -- never going to exist, and the server's own wording says what was
  -- wrong with it.
  it     <- evalIO (fake True)
  evalIO (it.arrive (Right refusal))
  answer <- evalIO (joining quiet it.connection 77 game)
  case answer of
    Right _ -> annotate "it handed back a game that was refused" >> failure
    Left why ->
      why === "This bot only plays on these board sizes: 9x9, 13x13, 19x19."

prop_nothingIsHandedBackUntilTheServerSaysWhereTheGameStands :: Property
prop_nothingIsHandedBackUntilTheServerSaysWhereTheGameStands =
  withTests 1 . property $ do
    -- The board here is a guess until it does. A window handed a game
    -- to play on a guess lets somebody move on it, and the moves go to
    -- a game that might not even exist.
    it     <- evalIO (fake True)
    answer <- evalIO (timeout 500000 (joining quiet it.connection 77 game))
    case answer of
      Just _  -> annotate "it handed back a game nobody had described" >> failure
      Nothing -> success
    -- And it did ask, so what it is waiting for is on its way.
    packets <- evalIO it.sent
    namesOf packets === ["game/connect"]

tests :: Group
tests = $$(discover)