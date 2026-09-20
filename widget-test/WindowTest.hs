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

import           Data.List                      ( isInfixOf )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified GI.Adw                        as Adw
import qualified GI.Gio                        as Gio
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple  ( Transition(..) )
import           GI.Gtk.Declarative.EventSource
import           Hedgehog

import           Go.Types
import           Stones.App
import           Stones.Chat                    ( chatPane )
import           Stones.Opponent
import           Stones.Session
import           Stones.Setup
import           WidgetUtils

-- | An opponent that does nothing and says so.
silent :: Opponent
silent = Opponent { name  = "nobody"
                  , tell  = \_ -> pure (Right ())
                  , hear  = pure ()
                  , leave = pure ()
                  , talks = True
                  , again = Just "nobody"
                  }

source :: Opponents
source = Opponents { open  = \_ -> pure (Right silent)
                   , close = \_ -> pure ()
                   , take' = \_ -> pure (Right silent)
                   , enter = pure (Right "nobody")
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
asking = Setup { against = Computer, size = 9, human = Black, strength = Strength 10, handicap = 0, speed = Live, ranked = False, handicapping = Standard, versus = "" }

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
  titles === [("Your move", aboutGame (theGame oneGame))]

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
  titles === [("Thinking\8230", aboutGame (theGame played))]

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
    watching <- subscribe (view' twoGames) made (const (pure ()))
    cancel watching
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
  -- A row and a toggle group for each thing there is to choose.
  rows === length choices
  groups === length choices
  titles === [("New game", describeSetup asking)]

prop_thePageOffersEveryChoiceThereIs :: Property
prop_thePageOffersEveryChoiceThereIs = withTests 1 . property $ do
  -- Every board, colour, strength and handicap there is, as toggles
  -- rather than as anything the page had to be told about twice. The
  -- count comes from the lists themselves, so a choice added to one of
  -- them is a choice this asks for.
  toggles <- evalIO . built onePage $ \widget' -> do
    groups <- descendantsOf Adw.ToggleGroup widget'
    traverse Adw.toggleGroupGetNToggles groups
  toggles === map fromIntegral choices

-- | How many there are of each thing the page asks about, in the order
-- the page asks, for a game on this machine.
choices :: [Int]
choices =
  [length places, length boardWidths, 2, length strengths, length handicaps]

-- | The same page, asking about a game on the server instead.
onlinePage :: State
onlinePage = next onePage (InSetup 1 (ChosePlace Online))

prop_thePageAsksOnlyWhatThePlaceItIsAboutCanAnswer :: Property
prop_thePageAsksOnlyWhatThePlaceItIsAboutCanAnswer =
  withTests 1 . property $ do
    -- A game somebody else pairs settles the colour, the opponent and
    -- the handicap on the other side, so those rows are not there at
    -- all. A row that did nothing would be worse than no row.
    (rows, groups, titles) <- evalIO . built onlinePage $ \widget' -> do
      rows   <- descendantsOf Adw.ActionRow widget'
      groups <- descendantsOf Adw.ToggleGroup widget'
      titles <- traverse Adw.preferencesRowGetTitle rows
      pure (length rows, length groups, titles)
    -- Five things to choose from, and the account, which has a button
    -- on it rather than a group of toggles. The name is typed into a
    -- row of its own kind, counted below.
    rows === 6
    groups === 5
    titles === ["Play", "Play on", "Speed", "Rank", "Handicap", "Account"]
    typed <- evalIO . built onlinePage $ \widget' ->
      length <$> descendantsOf Adw.EntryRow widget'
    typed === 1

prop_theOpponentRowShowsTheNameThePageHolds :: Property
prop_theOpponentRowShowsTheNameThePageHolds = withTests 1 . property $ do
  -- A row that showed nothing while the page held a name would be a
  -- row that lied, and the game would be asked for against somebody
  -- the player could not see. The command line can put one there
  -- before anybody has typed anything.
  let named = next onlinePage (InSetup 1 (ChoseVersus "amybot-beginner"))
  shown' <- evalIO . built named $ \widget' -> do
    rows <- descendantsOf Adw.EntryRow widget'
    traverse Gtk.editableGetText rows
  shown' === ["amybot-beginner"]
  -- And an empty one stays empty, which is what asks for anybody.
  blank <- evalIO . built onlinePage $ \widget' -> do
    rows <- descendantsOf Adw.EntryRow widget'
    traverse Gtk.editableGetText rows
  blank === [""]

prop_thePageSaysWhetherAnybodyIsSignedIn :: Property
prop_thePageSaysWhetherAnybodyIsSignedIn = withTests 1 . property $ do
  -- The line is on the row rather than anywhere else, because the row
  -- is where the button that changes it is.
  let signedInAs name = next onlinePage (SignedInAs (Right name))
  out <- evalIO (subtitlesOf onlinePage)
  inn <- evalIO (subtitlesOf (signedInAs "hikaru"))
  bad <- evalIO
    (subtitlesOf (next onlinePage (SignedInAs (Left "it was refused"))))
  out === ["Not signed in"]
  inn === ["Signed in as hikaru"]
  bad === ["it was refused"]

-- | What the rows of a page say under their titles, leaving out the
-- rows that say nothing.
subtitlesOf :: State -> IO [Text]
subtitlesOf state = built state $ \widget' -> do
  rows <- descendantsOf Adw.ActionRow widget'
  said <- traverse Adw.actionRowGetSubtitle rows
  pure [ line | Just line <- said, not (Text.null line) ]

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

-- | The window with its game played out to the end and stopped, which
-- is when the board marks groups instead of taking moves.
beingCounted :: State
beingCounted = foldl (\state event -> next state (InTab 1 event))
                 oneGame
                 [ Clicked (Coord 0 0)
                 , Arrived (Played White (Play (Coord 8 8)))
                 , Arrived (Played White Pass)
                 , Passed
                 , Arrived Stopped
                 , Arrived (Removing [Coord 8 8])
                 ]

prop_aGameBeingCountedStillBuildsAWindow :: Property
prop_aGameBeingCountedStillBuildsAWindow = withTests 1 . property $ do
  -- The button at the right-hand end is a different button now, and a
  -- property or an icon GTK does not know only shows up here.
  (titles, areas, buttons) <- evalIO . built beingCounted $ \widget' -> do
    titles  <- titlesUnder widget'
    areas   <- descendantsOf Gtk.DrawingArea widget'
    buttons <- descendantsOf Gtk.Button widget'
    pure (titles, length areas, length buttons)
  titles === [("Count the game", "nobody  \183  Click a group to mark it dead, then accept")]
  areas === 1
  assert (buttons >= 2)

prop_theGameStoppingPatchesTheWindow :: Property
prop_theGameStoppingPatchesTheWindow = withTests 1 . property $ do
  -- Pass and Accept are the same button in the same place, so the bar
  -- is patched rather than torn down and built again.
  said <- evalIO . runUI $ do
    made <- create (view' oneGame)
    pure (decision made (view' oneGame) (view' beingCounted))
  said === Modified

-- | The same one game, with the pane beside the board out and
-- something said in it.
talking :: State
talking = foldl
  (\state event -> next state event)
  oneGame
  [ InTab 1 (Arrived (Spoke "hikaru" "good game"))
  , InTab 1 (Arrived (Spoke "nobody" "thanks, you too"))
  , ChatToggled True
  ]

prop_thePaneShowsWhatWasSaidAndALineToSayOn :: Property
prop_thePaneShowsWhatWasSaidAndALineToSayOn = withTests 1 . property $ do
  (entries, labels) <- evalIO . built talking $ \widget' -> do
    entries <- descendantsOf Gtk.Entry widget'
    labels  <- traverse Gtk.labelGetLabel =<< descendantsOf Gtk.Label widget'
    pure (length entries, labels)
  -- One line to type on.
  entries === 1
  -- Who said each thing and what it was, in the order it was said.
  assert (["hikaru", "good game", "nobody", "thanks, you too"] `isInfixOf` labels)

prop_aGameNobodyCanBeTalkedToInHasNoPaneAndNoButtonForOne :: Property
prop_aGameNobodyCanBeTalkedToInHasNoPaneAndNoButtonForOne =
  withTests 1 . property $ do
    -- A program takes what is said to it and says nothing back. The
    -- pane is not hidden, it is not built: there is nothing to reveal.
    let quiet = next (next onePage (InSetup 1 StartPressed))
                     (TabOpened 1 (Right silent { talks = False }))
    (entries, toggles) <- evalIO . built (next quiet (ChatToggled True))
      $ \widget' -> do
          entries <- descendantsOf Gtk.Entry widget'
          toggles <- descendantsOf Gtk.ToggleButton widget'
          pure (length entries, length toggles)
    entries === 0
    -- And a game that can be talked to has one toggle button more,
    -- counted against the quiet one because the menu in the corner is
    -- built out of one of its own.
    talkative <- evalIO . built oneGame
      $ \widget' -> length <$> descendantsOf Gtk.ToggleButton widget'
    talkative === toggles + 1

prop_showingThePanePatchesTheWindow :: Property
prop_showingThePanePatchesTheWindow = withTests 1 . property $ do
  let said = next oneGame (InTab 1 (Arrived (Spoke "hikaru" "good game")))
  patched <- evalIO . runUI $ do
    made <- create (view' oneGame)
    pure (decision made (view' oneGame) (view' (next said (ChatToggled True))))
  patched === Modified

prop_thePaneStaysAtTheNewestMessage :: Property
prop_thePaneStaysAtTheNewestMessage = withTests 1 . property $ do
  -- More messages than fit, so there is something to scroll past.
  let many = [ Chat "hikaru" (Text.replicate 6 ("line " <> Text.pack (show n) <> " "))
             | n <- [1 .. 40 :: Int]
             ]
  (value, upper, showing) <- evalIO $ do
    window <- runUI $ do
      window <- Gtk.new
        Gtk.Window
        [#defaultWidth Gtk.:= 300, #defaultHeight Gtk.:= 300]
      made <- create (chatPane many)
      Gtk.windowSetChild window . Just =<< widgetOf made
      Gtk.windowPresent window
      pure window
    -- A few turns of the loop, which is what it takes to go from a
    -- window being presented to its contents being laid out and the
    -- scrolling that follows the laying out.
    mapM_ (const settle) [1 .. 20 :: Int]
    runUI $ do
      scrollers <- descendantsOf Gtk.ScrolledWindow =<< Gtk.toWidget window
      answer    <- case scrollers of
        scroller : _ -> do
          adjustment <- Gtk.scrolledWindowGetVadjustment scroller
          (,,)
            <$> Gtk.adjustmentGetValue adjustment
            <*> Gtk.adjustmentGetUpper adjustment
            <*> Gtk.adjustmentGetPageSize adjustment
        [] -> pure (0, 0, 0)
      Gtk.windowDestroy window
      pure answer
  annotateShow (value, upper, showing)
  -- There is more than fits, and what is in view is the end of it.
  assert (upper > showing)
  assert (value >= upper - showing - 1)

prop_theWindowIsUnderAnOverlayThatCanSpeakOverIt :: Property
prop_theWindowIsUnderAnOverlayThatCanSpeakOverIt =
  withTests 1 . property $ do
    -- Everything the window shows is under it, so something said about
    -- a game behind this one has somewhere to appear.
    overlays <- evalIO . built oneGame
      $ \widget' -> length <$> descendantsOf Adw.ToastOverlay widget'
    overlays === 1

prop_somethingSaidOutOfViewPatchesTheWindowRatherThanRebuildingIt
  :: Property
prop_somethingSaidOutOfViewPatchesTheWindowRatherThanRebuildingIt =
  withTests 1 . property $ do
    -- The overlay is the window's outermost widget, so a message that
    -- tore it down would take the board and the tabs with it.
    let heard = next (next oneGame NewTabPressed)
                     (InTab 1 (Arrived (Spoke "hikaru" "good game")))
    said <- evalIO . runUI $ do
      made <- create (view' oneGame)
      pure (decision made (view' oneGame) (view' heard))
    said === Modified

prop_aTabSaysWhoItIsAgainstAndHowLongIsLeft :: Property
prop_aTabSaysWhoItIsAgainstAndHowLongIsLeft = withTests 1 . property $ do
  -- Several games at once and one of them about to run out is the
  -- thing worth glancing across a row of tabs for.
  let clock = Clock { black   = Time { seconds = 598, periods = 0, each = 0 }
                    , white   = Time { seconds = 61, periods = 0, each = 0 }
                    , running = Just Black
                    }
      timed = next oneGame (InTab 1 (Arrived (Ticked clock)))
  titles <- evalIO (tabTitlesOf timed)
  -- This player has Black in this game, so it is Black's clock that
  -- says how long they have.
  titles === ["nobody  \183  9:58"]
  -- A game nobody is timing says who, and nothing else.
  plain <- evalIO (tabTitlesOf oneGame)
  plain === ["nobody"]
  -- And a game that has ended does not say how long is left in it,
  -- because that is not news about a game nobody is playing.
  done <- evalIO (tabTitlesOf (next timed (InTab 1 ResignPressed)))
  done === ["nobody"]

-- | What the tabs of a window are called.
tabTitlesOf :: State -> IO [Text]
tabTitlesOf state = do
  widget' <- runUI (widgetOf =<< create (view' state))
  settle
  runUI $ do
    views <- descendantsOf Adw.TabView widget'
    case views of
      [view] -> do
        count <- Adw.tabViewGetNPages view
        traverse
          (\at -> Adw.tabPageGetTitle =<< Adw.tabViewGetNthPage view at)
          [0 .. count - 1]
      _ -> pure []

prop_anOfferIsShownOverTheBoardWithAWayToTakeIt :: Property
prop_anOfferIsShownOverTheBoardWithAWayToTakeIt =
  withTests 1 . property $ do
    -- It has to survive being built, because a toast with a button on
    -- it is the one kind this window had never made.
    let offer = Invitation { from  = "lenzuru"
                           , about = asking { against = Online }
                           , token = 36052608
                           , game  = 90881581
                           }
        asked = next oneGame (InTab 1 (Arrived (Invited offer)))
    (titles, areas) <- evalIO . built asked $ \widget' -> do
      titles <- titlesUnder widget'
      areas  <- descendantsOf Gtk.DrawingArea widget'
      pure (titles, length areas)
    -- The window underneath is untouched: same board, same bar.
    titles === [("Your move", aboutGame (theGame asked))]
    areas === 1

prop_anOfferArrivingPatchesTheWindow :: Property
prop_anOfferArrivingPatchesTheWindow = withTests 1 . property $ do
  -- The overlay is the window's outermost widget, so an offer that
  -- tore it down would take the board and the tabs with it.
  let offer = Invitation { from  = "lenzuru"
                         , about = asking { against = Online }
                         , token = 36052608
                         , game  = 90881581
                         }
      asked = next oneGame (InTab 1 (Arrived (Invited offer)))
  said <- evalIO . runUI $ do
    made <- create (view' oneGame)
    pure (decision made (view' oneGame) (view' asked))
  said === Modified

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