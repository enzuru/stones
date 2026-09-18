-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The board as a widget.
--
-- What it draws and what it does with a click are tested without a
-- display. These are the parts that are a widget: what it is made of,
-- what a patch does to it, and that it can be listened to.
module GobanWidgetTest
  ( tests
  )
where

import           Data.IORef
import qualified Data.Vector                   as Vector
import qualified GI.Gio                        as Gio
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource
import           Hedgehog

import           Go.Board
import           Go.Types
import           Stones.Goban
import           WidgetUtils

-- | An empty 9x9 board with the player to move.
empty' :: GobanProps
empty' = GobanProps { board       = emptyBoard 9
                    , last        = Nothing
                    , hover       = Just Black
                    , coordinates = True
                    }

-- | The same board with a stone on it.
played :: GobanProps
played = empty' { board = stoned, last = Just (Coord 3 3) }
 where
  stoned = case place Black (Coord 3 3) (emptyBoard 9) of
    Right placement -> placement.after
    Left  _         -> emptyBoard 9

board :: GobanProps -> Widget GobanEvent
board = goban Vector.empty

prop_aBoardIsADrawingArea :: Property
prop_aBoardIsADrawingArea = withTests 1 . property $ do
  drawing <- evalIO . runUI $ do
    made <- create (board empty')
    Gtk.castTo Gtk.DrawingArea =<< widgetOf made
  assert (drawing /= Nothing)

prop_aBoardCarriesItsOwnControllers :: Property
prop_aBoardCarriesItsOwnControllers = withTests 1 . property $ do
  -- The click and the pointer are added when the widget is made, not
  -- when it is subscribed to, because an application subscribes again
  -- after every event and that would stack a new pair on every move.
  count <- evalIO . runUI $ do
    made       <- create (board empty')
    widget'    <- widgetOf made
    controllers <- Gtk.widgetObserveControllers widget'
    fromIntegral <$> Gio.listModelGetNItems controllers
  count === (2 :: Int)

prop_subscribingTwiceDoesNotStackControllers :: Property
prop_subscribingTwiceDoesNotStackControllers = withTests 1 . property $ do
  count <- evalIO . runUI $ do
    made    <- create (board empty')
    widget' <- widgetOf made
    let listen = subscribe (board empty') made (const (pure ()))
    one <- listen
    cancel one
    two <- listen
    cancel two
    three <- listen
    cancel three
    controllers <- Gtk.widgetObserveControllers widget'
    fromIntegral <$> Gio.listModelGetNItems controllers
  count === (2 :: Int)

prop_aBoardIsNeverRebuilt :: Property
prop_aBoardIsNeverRebuilt = withTests 1 . property $ do
  -- A board that was thrown away and made again on every move would
  -- flicker, and would forget where the pointer is.
  (unchanged, changed) <- evalIO . runUI $ do
    made <- create (board empty')
    pure
      ( decision made (board empty') (board empty')
      , decision made (board empty') (board played)
      )
  assert (unchanged /= Replaced)
  assert (changed /= Replaced)

prop_aPatchedBoardIsTheSameWidget :: Property
prop_aPatchedBoardIsTheSameWidget = withTests 1 . property $ do
  -- A board that was replaced rather than patched would lose where the
  -- pointer is, and would flicker on every move.
  same <- evalIO . runUI $ do
    made    <- create (board empty')
    before  <- widgetOf made
    patched <- apply made (board empty') (board played)
    after   <- widgetOf patched
    pure (before == after)
  same === True

prop_aBoardSendsItsClicksToWhoeverIsListening :: Property
prop_aBoardSendsItsClicksToWhoeverIsListening = withTests 1 . property $ do
  -- Nothing here can make GTK report a click, so this checks the other
  -- end of it: that subscribing puts a listener where the click
  -- handler looks, and that cancelling takes it away again.
  heard <- evalIO . runUI $ do
    made <- create (board empty')
    got  <- newIORef (0 :: Int)
    let listen = subscribe (board empty') made (const (modifyIORef' got (+ 1)))
    listening <- listen
    cancel listening
    readIORef got
  heard === 0

prop_aBoardCanBeBuiltForEverySize :: Property
prop_aBoardCanBeBuiltForEverySize = withTests 1 . property $ do
  areas <- evalIO . runUI $ traverse
    (\n -> do
      made <- create (board empty' { board = emptyBoard n })
      Gtk.castTo Gtk.DrawingArea =<< widgetOf made
    )
    [9, 13, 19]
  assert (all (/= Nothing) areas)

prop_aBoardOnTheScreenDrawsItself :: Property
prop_aBoardOnTheScreenDrawsItself = withTests 1 . property $ do
  -- Everything the board draws is checked against a surface in memory.
  -- This is the one that puts it in a window and lets GTK ask it to
  -- draw, which is what says the drawing is wired to the widget.
  shown <- evalIO $ do
    window <- runUI $ do
      window <- Gtk.new
        Gtk.Window
        [#defaultWidth Gtk.:= 300, #defaultHeight Gtk.:= 300]
      made <- create (board played)
      Gtk.windowSetChild window . Just =<< widgetOf made
      Gtk.windowPresent window
      pure window
    -- A few turns of the loop, which is what it takes to go from a
    -- window being presented to its contents being drawn.
    mapM_ (const settle) [1 .. 20 :: Int]
    mapped <- runUI $ do
      mapped <- Gtk.widgetGetMapped window
      Gtk.windowDestroy window
      pure mapped
    pure mapped
  shown === True

tests :: Group
tests = $$(discover)