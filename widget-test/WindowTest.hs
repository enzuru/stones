-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The window as GTK builds it.
--
-- What each event does to the state is tested without a display. This
-- is the other half: that the state the window is in can be turned
-- into widgets at all, that the widgets say what the state says, and
-- that a change patches the window rather than building a new one. A
-- property name GTK does not know, or an icon that is not there, is
-- the kind of mistake that only shows up here.
module WindowTest
  ( tests
  )
where

import           Data.Text                      ( Text )
import qualified GI.Adw                        as Adw
import qualified GI.Gio                        as Gio
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple  ( Transition(..) )
import           GI.Gtk.Declarative.EventSource
import           Hedgehog

import           Go.Types
import           Stones.App
import           Stones.Engine
import           Stones.Session
import           Stones.Setup
import           WidgetUtils

-- | An opponent that does nothing and says so.
silent :: Engine
silent = Engine { name    = "nobody"
                , newGame = \_ -> pure (Right ())
                , notify  = \_ _ -> pure (Right ())
                , genMove = \_ -> pure (Right Pass)
                , undo    = \_ -> pure (Right ())
                , score   = pure (Right "0")
                , close   = pure ()
                }

source :: Opponents
source = Opponents { open  = \_ _ -> pure (Right silent)
                   , close = \_ -> pure ()
                   }

-- | Where the window lands after an event.
next :: State -> Event -> State
next state event = case update' state event of
  Transition state' _ -> state'
  Exit                -> error "the transition exited"

-- | A window with nothing open in it.
noGames :: State
noGames = startingState source asking

-- | What the page of a new tab asks for here.
asking :: Setup
asking = Setup { size = 9, human = Black, strength = Fierce }

-- | A window with one tab, still on the page that asks what to play.
onePage :: State
onePage = next noGames NewTabPressed

-- | A window with one 9x9 game, whose opponent has started.
oneGame :: State
oneGame = next (next onePage (InSetup 1 StartPressed)) (TabOpened 1 (Right silent))

-- | A window with a second game beside it.
twoGames :: State
twoGames = next (next (next oneGame NewTabPressed) (InSetup 2 StartPressed))
                (TabOpened 2 (Right silent))

-- | Build the window for a state and hand it and its widget over.
built :: State -> (Gtk.Widget -> IO a) -> IO a
built state use = runUI $ do
  made <- create (view' state)
  use =<< widgetOf made

prop_aWindowWithNoGamesIsStillAWindow :: Property
prop_aWindowWithNoGamesIsStillAWindow = withTests 1 . property $ do
  titles <- evalIO (built noGames titlesUnder)
  titles === [("Stones", "")]

prop_theWindowSaysWhereTheGameStands :: Property
prop_theWindowSaysWhereTheGameStands = withTests 1 . property $ do
  titles <- evalIO (built oneGame titlesUnder)
  titles === [("Your move", capturesOf (theGame oneGame))]

prop_theWindowIsBuiltOfTheWidgetsItSaysItIs :: Property
prop_theWindowIsBuiltOfTheWidgetsItSaysItIs = withTests 1 . property $ do
  (bars, views, areas, buttons) <- evalIO . built twoGames $ \widget' -> do
    bars    <- descendantsOf Adw.HeaderBar widget'
    views   <- descendantsOf Adw.TabView widget'
    areas   <- descendantsOf Gtk.DrawingArea widget'
    buttons <- descendantsOf Gtk.Button widget'
    pure (length bars, length views, length areas, length buttons)
  bars === 1
  views === 1
  -- A board for each game, both built, whichever one is showing.
  areas === 2
  -- Undo, pass, and the menu, which is a button of its own kind.
  assert (buttons >= 2)

prop_theTabBarIsPointedAtTheTabView :: Property
prop_theTabBarIsPointedAtTheTabView = withTests 1 . property $ do
  -- The bar and the view are in different parts of the tree and are
  -- joined by name, so a name that does not match leaves a bar showing
  -- nothing.
  joined <- evalIO $ do
    widget' <- runUI (widgetOf =<< create (view' twoGames))
    -- The name is looked up on the next turn of the main loop, by
    -- which time the view the bar is named after has been built.
    settle
    runUI $ do
      bars <- descendantsOf Adw.TabBar widget'
      case bars of
        [bar] -> Adw.tabBarGetView bar
        _     -> pure Nothing
  assert (joined /= Nothing)

prop_aGameGoingOnPatchesTheWindowRatherThanRebuildingIt :: Property
prop_aGameGoingOnPatchesTheWindowRatherThanRebuildingIt =
  withTests 1 . property $ do
    let played = next oneGame (InTab 1 (Clicked (Coord 3 3)))
    said <- evalIO . runUI $ do
      made <- create (view' oneGame)
      pure (decision made (view' oneGame) (view' played))
    said === Modified

prop_openingATabPatchesTheWindow :: Property
prop_openingATabPatchesTheWindow = withTests 1 . property $ do
  said <- evalIO . runUI $ do
    made <- create (view' oneGame)
    pure (decision made (view' oneGame) (view' twoGames))
  said === Modified

prop_aPatchedWindowShowsTheNewGame :: Property
prop_aPatchedWindowShowsTheNewGame = withTests 1 . property $ do
  let played = next oneGame (InTab 1 (Clicked (Coord 3 3)))
  titles <- evalIO . runUI $ do
    made    <- create (view' oneGame)
    patched <- apply made (view' oneGame) (view' played)
    titlesUnder =<< widgetOf patched
  titles === [("Thinking\8230", capturesOf (theGame played))]

prop_aWindowWithABrokenGameSaysWhy :: Property
prop_aWindowWithABrokenGameSaysWhy = withTests 1 . property $ do
  let broken = next (next onePage (InSetup 1 StartPressed))
                    (TabOpened 1 (Left "no such program"))
  titles <- evalIO (built broken titlesUnder)
  titles === [("Stopped", "no such program")]

prop_aWindowCanBeListenedToAndLetGo :: Property
prop_aWindowCanBeListenedToAndLetGo = withTests 1 . property $ do
  evalIO . runUI $ do
    made      <- create (view' twoGames)
    listening <- subscribe (view' twoGames) made (const (pure ()))
    cancel listening
  success

prop_aTabOnItsPageShowsThePage :: Property
prop_aTabOnItsPageShowsThePage = withTests 1 . property $ do
  -- A tab opens on the page that asks what to play, so there is a
  -- status page and its buttons, and no board at all.
  (pages, areas, rows, groups, titles) <- evalIO . built onePage $ \widget' -> do
    pages  <- descendantsOf Adw.StatusPage widget'
    areas  <- descendantsOf Gtk.DrawingArea widget'
    rows   <- descendantsOf Adw.ActionRow widget'
    groups <- descendantsOf Adw.ToggleGroup widget'
    titles <- titlesUnder widget'
    pure (length pages, length areas, length rows, length groups, titles)
  pages === 1
  areas === 0
  -- A row and a toggle group for each of the three things to choose.
  rows === 3
  groups === 3
  titles === [("New game", describeSetup asking)]

prop_thePageOffersEveryChoiceThereIs :: Property
prop_thePageOffersEveryChoiceThereIs = withTests 1 . property $ do
  -- Three boards, two colours and three strengths, as toggles rather
  -- than as anything the page had to be told about twice.
  toggles <- evalIO . built onePage $ \widget' -> do
    groups <- descendantsOf Adw.ToggleGroup widget'
    traverse Adw.toggleGroupGetNToggles groups
  toggles === [3, 2, 3]

prop_startingAGameTurnsThePageIntoABoard :: Property
prop_startingAGameTurnsThePageIntoABoard = withTests 1 . property $ do
  (said, areas) <- evalIO . runUI $ do
    made    <- create (view' onePage)
    let said = decision made (view' onePage) (view' oneGame)
    patched <- apply made (view' onePage) (view' oneGame)
    areas   <- descendantsOf Gtk.DrawingArea =<< widgetOf patched
    pure (said, length areas)
  -- The window is patched rather than thrown away, and what was a page
  -- is a board.
  said === Modified
  areas === 1

prop_theMenuComesFromTheMarkup :: Property
prop_theMenuComesFromTheMarkup = withTests 1 . property $ do
  -- The menu is a GMenuModel read from data/ui/menu.ui. A file that
  -- cannot be read leaves the button with nothing behind it, and the
  -- window still comes up, so the only way to know is to look.
  sections <- evalIO . built oneGame $ \widget' -> do
    buttons <- descendantsOf Gtk.MenuButton widget'
    case buttons of
      [button] -> do
        model <- Gtk.menuButtonGetMenuModel button
        traverse Gio.menuModelGetNItems model
      _ -> pure Nothing
  -- One section for New Game and one for Resign.
  sections === Just 2

-- | The game showing in a window, for the tests that compare against
-- what its own subtitle should say.
theGame :: State -> Session
theGame state = case state.openTabs of
  ((_, Playing session) : _) -> session
  _                          -> error "no game in the first tab"

-- | Silence the unused import warning for Text, which the signatures
-- above do not name but the titles are.
_unusedText :: Text
_unusedText = ""

tests :: Group
tests = $$(discover)