-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | GNU Go as an opponent.
--
-- GNU Go speaks the Go Text Protocol when it is started with
-- @--mode gtp@, so this is "Stones.Engine.Gtp" with the commands of a
-- game written out and the answers read back as moves.
module Stones.Engine.GnuGo
  ( Level(..)
  , defaultLevel
  , levelRange
  , open
  , probe
  , withGnuGo
  )
where

import           Control.Exception              ( SomeException
                                                , bracket
                                                , try
                                                )
import           Data.Foldable                  ( for_ )
import           Data.IORef
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           Go.Types
import           Go.Vertex
import           Stones.Engine
import qualified Stones.Engine.Gtp             as Gtp

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

-- | Start GNU Go and hand back an opponent that talks to it.
--
-- The board size is remembered here, because a vertex cannot be read
-- without it: @A1@ is the bottom left corner of whatever board is in
-- play, and the row number is counted from the bottom.
open :: FilePath -> Level -> Int -> IO (Either Text Engine)
open program (Level level) initialSize = do
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
      current <- newIORef initialSize
      let engine = Engine { engineName    = "GNU Go"
                          , engineNewGame = newGame gtp current
                          , engineNotify  = notify gtp current
                          , engineGenMove = genMove gtp current
                          , engineUndo    = undo gtp
                          , engineScore   = score gtp
                          , engineClose   = Gtp.stop gtp
                          }
      setUp <- newGame gtp current initialSize
      pure $ case setUp of
        Left failure -> Left failure
        Right ()     -> Right engine
 where
  arguments =
    ["--mode", "gtp", "--level", show level]

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
  report
    (Gtp.command_
      gtp
      ("play " <> colorName color <> " " <> moveToVertex n move)
    )

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
probe :: FilePath -> Level -> IO (Either Text ())
probe program level = open program level 9 >>= \case
  Left  why    -> pure (Left why)
  Right engine -> Right () <$ engineClose engine

-- | Opponents that are GNU Go processes, for as long as this action
-- runs.
--
-- Every process started here is written down, and whatever is still
-- running when the action ends is stopped. A tab that closes stops its
-- own opponent, and stopping one twice does nothing, so a process is
-- never left behind and never waited on twice.
withGnuGo :: FilePath -> Level -> (Opponents -> IO a) -> IO a
withGnuGo program level use = bracket (newIORef []) stopEveryone $ \running ->
  use Opponents { openOpponent  = openOne running
                , closeOpponent = engineClose
                }
 where
  openOne running n = open program level n >>= \case
    Left  why    -> pure (Left why)
    Right engine -> Right engine <$ modifyIORef' running (engine :)

  stopEveryone running = do
    started <- readIORef running
    for_ started engineClose
