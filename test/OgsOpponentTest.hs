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

-- | Join it, on a connection that takes what it is given.
joined :: IO (Fake, Opponent)
joined = do
  it     <- fake True
  answer <- joining it.connection game
  case answer of
    Left  why -> error (show why)
    Right them -> pure (it, them)

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

-- | The payload of an event that was sent.
bodyOf :: Packet -> Maybe Value
bodyOf = \case
  Event _ (body : _) _ -> Just body
  _                    -> Nothing

-- | The name of an event that was sent.
namesOf :: [Packet] -> [Text]
namesOf packets = [ name | Event name _ _ <- packets ]

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
  answer <- evalIO (joining it.connection game)
  case answer of
    Left why -> why === "the connection has closed"
    Right _  -> annotate "it joined a game on a closed connection" >> failure

prop_aMoveThatArrivesComesBackAsOne :: Property
prop_aMoveThatArrivesComesBackAsOne = withTests 1 . property $ do
  (it, them) <- evalIO joined
  evalIO (it.arrive (Right (moveEvent 0 3 3)))
  evalIO (it.arrive (Right (moveEvent 1 15 3)))
  arrived <- evalIO (heardFrom them 2)
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
  evalIO (it.arrive (Right (moveEvent 0 3 3)))
  arrived <- evalIO (heardFrom them 1)
  arrived === Just [Played Black (Play (Coord 3 3))]

prop_theConnectionGoingEndsWhatIsHeard :: Property
prop_theConnectionGoingEndsWhatIsHeard = withTests 1 . property $ do
  -- A game learns its opponent has gone from the stream ending, and
  -- the last thing on it is why.
  (it, them) <- evalIO joined
  evalIO (it.arrive (Left "the connection closed"))
  evalIO it.finish
  arrived <- evalIO (timeout 3000000 (Pipes.toListM them.hear))
  arrived === Just [Lost "the connection closed"]

prop_theCountOfMovesFollowsTheBoard :: Property
prop_theCountOfMovesFollowsTheBoard = withTests 1 . property $ do
  -- Anything said about a game carries where the game was when it was
  -- said, so the count has to follow every move, this program's own
  -- included, because the server says them all.
  (it, them) <- evalIO joined
  evalIO (it.arrive (Right (moveEvent 0 3 3)))
  evalIO (it.arrive (Right (moveEvent 1 15 3)))
  _ <- evalIO (heardFrom them 2)
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

tests :: Group
tests = $$(discover)
