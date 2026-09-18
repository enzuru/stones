-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
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
import           Stones.Setup

-- | An opponent that passes whatever it is asked, so that the board
-- comes back to the player after every move.
passer :: Engine
passer = Engine { name    = "passer"
                , newGame = \_ -> pure (Right ())
                , notify  = \_ _ -> pure (Right ())
                , genMove = \_ -> pure (Right Pass)
                , undo    = \_ -> pure (Right ())
                , score   = pure (Right "0")
                , close   = pure ()
                }

source :: Opponents
source = Opponents { open  = \_ _ -> pure (Right passer)
                   , close = \_ -> pure ()
                   }

-- | The player's stones on the board of the first tab, by name.
--
-- The stones rather than the last move, because the opponent here
-- passes and a pass is not a point: the last move would go back to
-- nothing as soon as it answered.
stones :: State -> Text
stones state = case state.openTabs of
  ((_, Playing session) : _)
    | null named -> "-"
    | otherwise  -> Text.unwords named
   where
    board = session.game.board
    named = mapMaybe
      (toVertex (sessionSize session))
      [ point | point <- coords board, stoneAt board point == Just Black ]
  -- A tab still on its page, or no tab at all, has no stones on it.
  _ -> "-"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  -- The window is built of libadwaita widgets, and they want adw_init
  -- rather than gtk_init: the style manager the board reads is one of
  -- the things it puts in place.
  Adw.init
  let window = application source Setup { size     = 9
                                        , human    = Black
                                        , strength = Gentle
                                        }
  void (run window { update = reporting
                   , inputs = inputs window <> [past, quitWhenTold]
                   })
 where
  reporting state event = case update' state event of
    Exit                 -> Exit
    Transition state' go -> Transition state' (go <> say (stones state'))
  say there = perform $ do
    Text.putStrLn ("BOARD " <> there)
    hFlush stdout
    pure Nothing

-- | Press the button on the page the first tab opens on.
--
-- A tab opens on the page that asks what to play, and this test is
-- about clicking the board rather than about that page. The page is
-- driven from here so that the clicking below lands on a board, and it
-- goes through the same event the button sends.
past :: Producer Event IO ()
past = Pipes.yield (InSetup 1 StartPressed)

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
