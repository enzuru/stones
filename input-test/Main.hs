-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The window, driven with real clicks.
--
-- GTK 4 reports a click through a gesture on the widget, and nothing
-- in it can make one happen from code: there is no way to synthesise
-- the event. So the path from a click on the board to a stone on the
-- board is the one path the other tests cannot reach, and it is the
-- one the whole program is for.
--
-- This is the real window, with an opponent that passes whatever it is
-- asked, so that every click is the player's turn again. It prints
-- where the last stone went after every event, and
-- @tests\/gui-input.sh@ clicks at it and reads those lines back.
module Main
  ( main
  )
where

import           Control.Exception              ( IOException
                                                , try
                                                )
import           Control.Monad                  ( void )
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified GI.Adw                        as Adw
import           GI.Gtk.Declarative.App.Simple
import           Pipes                          ( Producer
                                                , lift
                                                )
import qualified Pipes
import           System.IO

import           Go.Board
import           Go.Game
import           Go.Types
import           Go.Vertex                      ( toVertex )
import           Stones.App
import           Stones.Engine
import           Stones.Session

-- | An opponent that passes whatever it is asked, so that the board
-- comes back to the player after every move.
passer :: Engine
passer = Engine { engineName    = "passer"
                , engineNewGame = \_ -> pure (Right ())
                , engineNotify  = \_ _ -> pure (Right ())
                , engineGenMove = \_ -> pure (Right Pass)
                , engineUndo    = \_ -> pure (Right ())
                , engineScore   = pure (Right "0")
                , engineClose   = pure ()
                }

source :: Opponents
source = Opponents { openOpponent  = \_ -> pure (Right passer)
                   , closeOpponent = \_ -> pure ()
                   }

-- | The player's stones on the board of the first tab, by name.
--
-- The stones rather than the last move, because the opponent here
-- passes and a pass is not a point: the last move would go back to
-- nothing as soon as it answered.
stones :: State -> Text
stones state = case stateGames state of
  ((_, session) : _)
    | null named -> "-"
    | otherwise  -> Text.unwords named
   where
    board = gameBoard (sessionGame session)
    named = mapMaybe
      (toVertex (sessionSize session))
      [ point | point <- coords board, stoneAt board point == Just Black ]
  [] -> "-"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  -- The window is built of libadwaita widgets, and they want adw_init
  -- rather than gtk_init: the style manager the board reads is one of
  -- the things it puts in place.
  Adw.init
  let window = application source Black 9
  void (run window { update = reporting
                   , inputs = inputs window <> [quitWhenTold]
                   })
 where
  reporting state event = case update' state event of
    Exit                 -> Exit
    Transition state' go -> Transition state' (go <> say (stones state'))
  say there = perform $ do
    Text.putStrLn ("BOARD " <> there)
    hFlush stdout
    pure Nothing

-- | Wait for a line on the standard input, and close the window when
-- one arrives or when there is no more input coming.
--
-- The window has to end of its own accord rather than be killed, or
-- the run time system never gets to write down what the run reached,
-- and @make coverage@ has nothing to add up.
quitWhenTold :: Producer Event IO ()
quitWhenTold = do
  _ <- lift (try (Text.hGetLine stdin) :: IO (Either IOException Text))
  Pipes.yield Closed
