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
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple  ( Transition(..) )
import           GI.Gtk.Declarative.EventSource
import           Hedgehog

import           Go.Types
import           Stones.App
import           Stones.Engine
import           Stones.Session
import           WidgetUtils

-- | An opponent that does nothing and says so.
silent :: Engine
silent = Engine { engineName    = "nobody"
                , engineNewGame = \_ -> pure (Right ())
                , engineNotify  = \_ _ -> pure (Right ())
                , engineGenMove = \_ -> pure (Right Pass)
                , engineUndo    = \_ -> pure (Right ())
                , engineScore   = pure (Right "0")
                , engineClose   = pure ()
                }

source :: Opponents
source = Opponents { openOpponent  = \_ -> pure (Right silent)
                   , closeOpponent = \_ -> pure ()
                   }

-- | Where the window lands after an event.
next :: State -> Event -> State
next state event = case update' state event of
  Transition state' _ -> state'
  Exit                -> error "the transition exited"

-- | A window with nothing open in it.
noGames :: State
noGames = startingState source Black

-- | A window with one 9x9 game, whose opponent has started.
oneGame :: State
oneGame = next (next noGames (NewTabPressed 9)) (TabOpened 1 (Right silent))

-- | A window with a second game beside it.
twoGames :: State
twoGames = next (next oneGame (NewTabPressed 19)) (TabOpened 2 (Right silent))

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
  let broken = next (next noGames (NewTabPressed 9))
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

-- | The game showing in a window, for the tests that compare against
-- what its own subtitle should say.
theGame :: State -> Session
theGame state = case stateGames state of
  ((_, session) : _) -> session
  []                 -> error "no games"

-- | Silence the unused import warning for Text, which the signatures
-- above do not name but the titles are.
_unusedText :: Text
_unusedText = ""

tests :: Group
tests = $$(discover)
