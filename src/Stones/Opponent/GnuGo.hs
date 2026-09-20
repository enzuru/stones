-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | GNU Go as an opponent.
--
-- GNU Go answers when it is asked and says nothing otherwise, so
-- everything it says is something this program asked for. It is given
-- a stream of its own even so, and puts its answers on that rather
-- than handing them back, because that is where a game on a server
-- would put them. The window above cannot tell the two apart.
module Stones.Opponent.GnuGo
  ( Level(..)
  , defaultLevel
  , levelRange
  , levelFor
  , open
  , probe
  , withGnuGo
  )
where

import           Control.Concurrent.STM         ( atomically )
import           Control.Exception              ( SomeException
                                                , bracket
                                                , try
                                                )
import           Data.Foldable                  ( for_ )
import           Data.IORef
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Pipes.Concurrent               ( Output
                                                , fromInput
                                                , send
                                                , spawn'
                                                , unbounded
                                                )

import           Go.Types
import           Go.Vertex
import           Stones.Opponent
import qualified Stones.Opponent.Gtp           as Gtp
import           Stones.Setup                   ( Setup(..)
                                                , Strength
                                                , strengthNumber
                                                )

-- | How hard GNU Go thinks. Its own range is 1 to 10, and 10 is what
-- it plays at when nothing is said.
newtype Level = Level Int
  deriving (Eq, Ord, Show)

-- | The level GNU Go plays at on its own.
defaultLevel :: Level
defaultLevel = Level 10

-- | The levels GNU Go accepts, weakest first.
levelRange :: (Int, Int)
levelRange = (1, 10)

-- | The level that answers a strength.
--
-- Three of GNU Go's ten: the weakest, the middle, and the one it plays
-- at when nothing is said.
levelFor :: Strength -> Level
levelFor asked = Level (max low (min high (strengthNumber asked)))
  where (low, high) = levelRange

-- | Start GNU Go, and hand back an opponent that talks to it.
--
-- The board size is remembered here, because a vertex cannot be read
-- without it: @A1@ is the bottom left corner of whatever board is in
-- play, and the row number is counted from the bottom.
open :: FilePath -> Setup -> IO (Either Text Opponent)
open program setup = do
  started <- try (Gtp.start program arguments)
  case started of
    Left failure -> pure
      (Left
        (  "Could not start "
        <> Text.pack program
        <> ": "
        <> Text.pack (show (failure :: SomeException))
        )
      )
    Right gtp -> do
      current             <- newIORef setup.size
      -- Unbounded, because the only writer is whoever is telling this
      -- opponent something, and a write that blocked would block the
      -- window's own job.
      (output, input, seal) <- spawn' unbounded
      let opponent = Opponent
            { name  = "GNU Go"
            , tell  = telling gtp current output setup.human
            , hear  = fromInput input
            , leave = Gtp.stop gtp >> atomically seal
              -- It takes what is said to it and says nothing back.
            , talks = False
              -- And it is nobody in particular. The next one started
              -- is the same one.
            , again = Nothing
            }
      setUp <- newGame gtp current setup.size
      case setUp of
        Left  failure -> pure (Left failure)
        Right ()      -> do
          placed <- placing gtp current output setup
          pure $ case placed of
            Left  failure -> Left failure
            Right ()      -> Right opponent
 where
  Level level = levelFor setup.strength
  arguments   = ["--mode", "gtp", "--level", show level]

-- | Put the handicap down, if the game has one.
--
-- GNU Go places the stones and says where it put them, and the game
-- above is told the same way a game on a server would be: the position
-- arrives before anything else does. Black has the stones, so White is
-- the one to move.
placing
  :: Gtp.Gtp -> IORef Int -> Output Heard -> Setup -> IO (Either Text ())
placing gtp current output setup
  | setup.handicap < 2 = pure (Right ())
  | otherwise = do
    n      <- readIORef current
    answer <- report
      (Gtp.command
        gtp
        ("fixed_handicap " <> Text.pack (show setup.handicap))
      )
    case answer of
      Left  failure -> pure (Left failure)
      Right text    -> case traverse (fromVertex n) (Text.words text) of
        Nothing ->
          pure (Left ("The engine answered with \"" <> text <> "\"."))
        Just stones -> do
          _ <- put
            output
            (Began Opening { width  = n
                           , stones = [ (Black, stone) | stone <- stones ]
                           , opens  = White
                           , yours  = setup.human
                           }
            )
          pure (Right ())

-- | Say one thing to GNU Go.
--
-- What comes back goes on the stream rather than into the answer. The
-- answer says whether the thing was said, which is a different
-- question: a move it will not take is a refusal, and a move it will
-- not make is news that arrives later.
telling
  :: Gtp.Gtp -> IORef Int -> Output Heard -> Color -> Told -> IO (Either Text ())
telling gtp current output human = \case
  Moved move -> notify gtp current human move
  TookBack n -> undo gtp n
  Asked      -> genMove gtp current them
    >>= put output . either Lost (Played them)
  Counting   -> score gtp >>= put output . either Lost Counted
  -- The protocol has no word for any of these. A resignation ends the
  -- game here and there is nothing left to ask GNU Go for, and GNU Go
  -- has nothing to say and nobody to say it to.
  Resigned   -> pure (Right ())
  Said _     -> pure (Right ())
  -- Nobody has to agree with GNU Go about which stones are dead. It
  -- counts the game itself, which is what @final_score@ is, so a game
  -- against it never stops to be counted by hand.
  Marking _ _ -> pure (Right ())
  Accepting _ -> pure (Right ())
  where them = opposite human

-- | Put something on the stream. A stream nobody is reading from any
-- more takes nothing, which is what a closed game looks like.
put :: Output Heard -> Heard -> IO (Either Text ())
put output heard = Right () <$ atomically (send output heard)

-- | Turn a failed command into the line the window shows.
report :: IO (Either Gtp.GtpError a) -> IO (Either Text a)
report action = either (Left . Gtp.describeGtpError) Right <$> action

newGame :: Gtp.Gtp -> IORef Int -> Int -> IO (Either Text ())
newGame gtp current n = do
  sized <- report (Gtp.command_ gtp ("boardsize " <> Text.pack (show n)))
  case sized of
    Left  failure -> pure (Left failure)
    Right ()      -> do
      writeIORef current n
      report (Gtp.command_ gtp "clear_board")

notify :: Gtp.Gtp -> IORef Int -> Color -> Move -> IO (Either Text ())
notify gtp current color move = do
  n <- readIORef current
  case moveToVertex n move of
    -- The board this program is playing on and the board the engine is
    -- playing on have come apart, because a move was made on a point
    -- that does not exist on the board the engine was last given.
    Nothing ->
      pure
        (Left
          (  "That move is not on the "
          <> Text.pack (show n)
          <> "x"
          <> Text.pack (show n)
          <> " board the engine was given."
          )
        )
    Just named -> report
      (Gtp.command_ gtp ("play " <> colorName color <> " " <> named))

genMove :: Gtp.Gtp -> IORef Int -> Color -> IO (Either Text Move)
genMove gtp current color = do
  n      <- readIORef current
  answer <- report (Gtp.command gtp ("genmove " <> colorName color))
  pure $ case answer of
    Left  failure -> Left failure
    Right text    -> case moveFromVertex n text of
      Just move -> Right move
      Nothing   -> Left ("The engine answered with \"" <> text <> "\".")

undo :: Gtp.Gtp -> Int -> IO (Either Text ())
undo gtp count
  | count <= 0 = pure (Right ())
  | otherwise = go count
 where
  go 0 = pure (Right ())
  go n = report (Gtp.command_ gtp "undo") >>= \case
    Left  failure -> pure (Left failure)
    Right ()      -> go (n - 1)

score :: Gtp.Gtp -> IO (Either Text Text)
score gtp = report (Gtp.command gtp "final_score")

-- | The word the protocol uses for a colour.
colorName :: Color -> Text
colorName Black = "black"
colorName White = "white"

-- | Make sure the program is there and speaks the protocol.
--
-- A window opens its first game a moment after it appears, and an
-- engine that is not there would show up as a tab that never starts.
-- This is what turns that into a line on the terminal and an exit
-- code, which is what somebody running the program from a script
-- wants.
probe :: FilePath -> Setup -> IO (Either Text ())
probe program setup = open program setup >>= \case
  Left  why      -> pure (Left why)
  Right opponent -> Right () <$ opponent.leave

-- | Opponents that are GNU Go processes, for as long as this action
-- runs.
--
-- Every process started here is written down, and whatever is still
-- running when the action ends is stopped. A tab that closes stops its
-- own opponent, and stopping one twice does nothing, so a process is
-- never left behind and never waited on twice.
withGnuGo :: FilePath -> (Opponents -> IO a) -> IO a
withGnuGo program use = bracket (newIORef []) stopEveryone $ \running ->
  use Opponents { open  = openOne running
                , close = (.leave)
                  -- There is nobody to be, so there is nothing to sign
                  -- in as. The window never asks this of a game on
                  -- this machine, and says so if it ever does.
                , take' = \_ ->
                  pure (Left "Nobody on this computer offers a game.")
                , enter = pure (Left "Games on this computer need no account.")
                }
 where
  openOne running setup = open program setup >>= \case
    Left  why      -> pure (Left why)
    Right opponent -> Right opponent <$ modifyIORef' running (opponent :)

  stopEveryone :: IORef [Opponent] -> IO ()
  stopEveryone running = do
    started <- readIORef running
    for_ started (.leave)
