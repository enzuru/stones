-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The window, which holds a game per tab.
--
-- Each tab has a game and an opponent of its own. An opponent holds one
-- board, so a second game means a second opponent rather than a second
-- question to the first, and closing a tab lets its opponent go.
--
-- Everything about one game is in "Stones.Session". This module is
-- about the tabs: which games are open, which one is showing, and
-- where each answer belongs.
module Stones.App
  ( State(..)
  , Event(..)
  , TabId
  , Stage(..)
  , startingState
  , update'
  , view'
  , application
    -- * The lines the header bar shows
  , statusOf
  , detailOf
  , stateOf
  , aboutGame
  , capturesOf
  )
where

import           Data.Bifunctor                 ( bimap )
import           Data.List                      ( find )
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vector

import qualified GI.Adw                        as Adw
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Adwaita.Bin ( )
import           GI.Gtk.Declarative.Adwaita.HeaderBar
                                                ( headerBarEnd
                                                , headerBarStart
                                                )
import           GI.Gtk.Declarative.Adwaita.References
                                                ( tabBarView )
import           GI.Gtk.Declarative.Adwaita.Slots
                                                ( titleWidget )
import           GI.Gtk.Declarative.Adwaita.TabView
import           GI.Gtk.Declarative.Adwaita.ToolbarView
                                                ( toolbarContent
                                                , toolbarTop
                                                )
import           GI.Gtk.Declarative.App.Simple
import qualified Pipes

import           Go.Game
import           Go.Types
import           Stones.Engine
import           Stones.Goban
import           Stones.Session
import           Stones.Setup

-- | Tells one tab from another, for as long as the window is open. A
-- number rather than the tab's place in the list, because tabs are
-- closed and made and dragged about, and a name that moves is a name
-- that sends an answer to the wrong game.
type TabId = Int

-- | How far along a tab is.
--
-- A tab opens on the page that asks what to play, and becomes a game
-- when somebody has answered. Nothing is started while that page is
-- up, so a window full of tabs nobody has started is a window with no
-- processes behind it.
--
-- It is not called @Tab@ or @Stage@, because the tab view has both of
-- those already.
data Stage
  = Choosing Setup
  | Playing Session

-- | Everything the window shows.
data State = State
  { openTabs  :: [(TabId, Stage)]
    -- ^ What is open, in the order the tabs are in.
  , showing   :: Maybe TabId
    -- ^ The tab the bars at the top are about.
  , nextTab   :: TabId
    -- ^ The name the next tab gets.
  , opening   :: Setup
    -- ^ What a new tab's page starts out asking for. It is the command
    -- line to begin with, and afterwards it is whatever was last
    -- started, so a second game of the same kind is one click.
  , opponents :: Opponents
    -- ^ Where a game gets its opponent, once there is a game.
  }

-- | What the window reports.
data Event
  = InTab TabId SessionEvent
    -- ^ Something happened to the game in this tab.
  | InSetup TabId SetupEvent
    -- ^ Somebody chose something on this tab's page, or started it.
  | TabOpened TabId (Either Text Engine)
    -- ^ The opponent a game was waiting for, or why there is none.
  | NewTabPressed
    -- ^ Open a tab, on the page that asks what to play.
  | TabSelected Text
  | TabClosePressed Text
  | TabsReordered (Vector Text)
  | Closed

-- | A window with no games in it yet.
--
-- The first game is not opened here. The window sends itself a
-- 'NewTabPressed' as its first event, so that the first tab and every
-- later one are the same piece of code, and so that the window is on
-- the screen while its first opponent is starting.
startingState :: Opponents -> Setup -> State
startingState opponents opening = State { openTabs  = []
                                        , showing   = Nothing
                                        , nextTab   = 1
                                        , opening   = opening
                                        , opponents = opponents
                                        }

-- * Updating
-------------

update' :: State -> Event -> Transition State Event
update' state = \case
  Closed                       -> Exit

  NewTabPressed                -> openTab state

  InSetup tab event            -> inSetup state tab event

  TabOpened tab (Right engine) -> inGame state tab (opened engine)
  TabOpened tab (Left  why   ) -> inGame state tab (stayAt . couldNotOpen why)

  InTab tab event              -> inGame state tab (`step` event)

  TabSelected key              -> Transition state { showing = tabOf state key } none

  TabClosePressed key          -> closeTab state key

  TabsReordered keys           -> Transition state { openTabs = inOrder } none
    where inOrder = mapMaybe (withId state) (Vector.toList keys)

-- | A game that has changed without anything being asked of anybody.
stayAt :: Session -> Played
stayAt session = Transition session none

-- | Open a tab, on the page that asks what to play.
--
-- No opponent is started. Nothing is started until somebody presses
-- the button on that page, because until then there is no game for one
-- to play.
openTab :: State -> Transition State Event
openTab state = Transition
  state { openTabs = state.openTabs <> [(tab, Choosing state.opening)]
        , showing = Just tab
        , nextTab = tab + 1
        }
  none
  where tab = state.nextTab

-- | Something happened on a tab's page.
--
-- A choice changes what the page is asking for. The button turns the
-- page into a game, and that is the one thing here that starts a
-- process.
inSetup :: State -> TabId -> SetupEvent -> Transition State Event
inSetup state tab event = case lookup tab state.openTabs of
  Just (Choosing setup) -> case event of
    StartPressed -> Transition
      (replacing state tab (Playing (starting setup.human setup.size)))
        -- What was started is what the next tab asks for first.
        { opening = setup
        }
      (perform
        (Just . TabOpened tab <$> state.opponents.open setup.size setup.strength)
      )
    _ -> Transition (replacing state tab (Choosing (chose setup event))) none
  _ -> Transition state none

-- | Do something to the game in one tab.
--
-- A game answers with a transition of its own, and this lifts it: the
-- new game goes back into the tab it came from, and its events are
-- addressed to that tab.
--
-- An event for a tab that is not there, or for one that is still a
-- page, is dropped. That is how an answer from an opponent whose tab
-- was closed while it was thinking ends: the opponent was stopped, the
-- answer it was in the middle of arrives as a failure, and there is no
-- longer a game it is about.
inGame :: State -> TabId -> (Session -> Played) -> Transition State Event
inGame state tab move = case lookup tab state.openTabs of
  Just (Playing session) ->
    bimap (replacing state tab . Playing) (InTab tab) (move session)
  _ -> Transition state none

-- | The window with one tab holding something else.
replacing :: State -> TabId -> Stage -> State
replacing state tab held = state
  { openTabs = [ (tab', if tab' == tab then held else other)
           | (tab', other) <- state.openTabs
           ]
  }

-- | Close a tab, and let its opponent go.
--
-- The window closes with its last tab. A window with no tabs in it
-- would have nothing to show and nothing to do, and a new game is a
-- new window away. The opponent of that last tab is not let go here,
-- because an ending window does nothing else afterwards: what stops it
-- is the same thing that stops the opponents of any tabs still open,
-- which is whatever handed this window its 'Opponents'.
closeTab :: State -> Text -> Transition State Event
closeTab state key = case withId state key of
  Nothing         -> Transition state none
  Just (tab, held) -> case remaining of
    [] -> Exit
    _  -> Transition state { openTabs = remaining, showing = showing' }
                     (release state.opponents held)
   where
    remaining = [ open | open <- state.openTabs, fst open /= tab ]
    -- Showing the tab that took the closed one's place, or the last
    -- one, which is what a tab bar does.
    showing' = case state.showing of
      Just showed | showed /= tab -> Just showed
      _                           -> fst <$> nextAfter tab state.openTabs

-- | Stop the opponent of a tab that has closed.
--
-- A tab that is still a page has none. An opponent that is thinking is
-- stopped in the middle of it. That is what stopping is for, and the
-- answer it was about to give arrives at a tab that is no longer
-- there, where it is dropped.
release :: Opponents -> Stage -> Cmd Event
release _         (Choosing _      ) = none
release opponents (Playing session) = case session.opponent of
  Idle    engine -> letGo engine
  Waiting engine -> letGo engine
  Starting       -> none
  Gone _         -> none
  where letGo engine = perform (Nothing <$ opponents.close engine)

-- | The tab after this one, or the one before it when this is the
-- last.
nextAfter :: TabId -> [(TabId, Stage)] -> Maybe (TabId, Stage)
nextAfter tab open = case break ((== tab) . fst) open of
  (before, _ : after) -> case after of
    next : _ -> Just next
    []       -> lastOf before
  _ -> Nothing
 where
  lastOf [] = Nothing
  lastOf xs = Just (last xs)

-- * The tabs
-------------

-- | What a tab is called in the markup, which is how one render is
-- matched with the next.
keyOf :: TabId -> Text
keyOf tab = "game-" <> Text.pack (show tab)

-- | The tab a key names.
tabOf :: State -> Text -> Maybe TabId
tabOf state key = fst <$> withId state key

-- | The tab a key names, and what is in it.
withId :: State -> Text -> Maybe (TabId, Stage)
withId state key = find ((== key) . keyOf . fst) state.openTabs

-- | The tab the bar at the top is about, and what is in it.
shown :: State -> Maybe (TabId, Stage)
shown state = do
  tab  <- state.showing
  held <- lookup tab state.openTabs
  pure (tab, held)

-- | The game the bar at the top is about, when the tab showing has one
-- rather than a page.
shownGame :: State -> Maybe (TabId, Session)
shownGame state = case shown state of
  Just (tab, Playing session) -> Just (tab, session)
  _                           -> Nothing

-- * The window
---------------

-- | The name the tab bar finds the tab view by.
viewName :: Text
viewName = "stones-games"

-- | The window is a header bar over a board, and nothing else.
--
-- What the player can do is in the bar rather than under the board:
-- the two moves that are made often are buttons at the start and the
-- end of it, everything else is in the menu, and what the game stands
-- at is the window's title and subtitle. That is the shape the GNOME
-- games have, and it leaves the whole of the window below the bar to
-- the board.
view' :: State -> AppView Adw.ApplicationWindow Event
view' state =
  bin
      Adw.ApplicationWindow
      [ #title := "Stones"
      , #defaultWidth := 760
      , #defaultHeight := 800
      , #widthRequest := 360
      , #heightRequest := 360
      , on #closeRequest (True, Closed)
      ]
    $ container
        Adw.ToolbarView
        []
        [ toolbarTop (header state)
        , toolbarTop (widget Adw.TabBar [tabBarView viewName])
        , toolbarContent (gameTabs state)
        ]

-- | The bar at the top: what the player does on the left and the
-- right, and where the game stands in the middle.
--
-- The title says whose turn it is and the subtitle carries the
-- numbers, which is why it is marked @numeric@: that style class asks
-- for digits of one width, so a capture count does not shuffle the
-- line about as it changes.
header :: State -> Widget Event
header state = container
  Adw.HeaderBar
  [ titleWidget
      (widget
        Adw.WindowTitle
        [ #title := maybe "Stones" (statusOf . snd) (shown state)
        , #subtitle := maybe "" (detailOf . snd) (shown state)
        , classes ["numeric"]
        ]
      )
  ]
  [ headerBarStart
    (action state "edit-undo-symbolic" "Undo" canUndo UndoPressed)
  -- Packed from the right edge inward, so the menu is the button in
  -- the corner, which is where a GNOME program keeps it.
  , headerBarEnd (mainMenu state)
  , headerBarEnd
    (action state
            "media-skip-forward-symbolic"
            "Pass, giving the move to the other player"
            playable
            Passed)
  ]

-- | A button in the header bar that does something to the game
-- showing.
--
-- A tab that is still a page has no game to do it to, and neither has
-- a window with no tabs, so both have the buttons and both have them
-- greyed.
action
  :: State -> Text -> Text -> (Session -> Bool) -> SessionEvent -> Widget Event
action state icon tip enabled event = case shownGame state of
  Nothing -> widget
    Gtk.Button
    [#iconName := icon, #tooltipText := tip, #sensitive := False]
  Just (tab, session) -> widget
    Gtk.Button
    [ #iconName := icon
    , #tooltipText := tip
    , #sensitive := enabled session
    , on #clicked (InTab tab event)
    ]

-- | The menu in the corner: what a game is opened with, and the one
-- thing a player does to a game that is not worth a button of its own.
mainMenu :: State -> Widget Event
mainMenu state = menuButton
  [ #iconName := "open-menu-symbolic"
  , #primary := True
  , #tooltipText := "Main Menu"
  ]
  -- The boards used to be here. They are on the page a tab opens on
  -- now, with the colour and the opponent beside them, which is where
  -- somebody choosing a game can see all three at once.
  [ menuSection Nothing [menuItem "New Game" NewTabPressed]
  , menuSection
    Nothing
    (Vector.fromList
      [ menuItem "Resign" (InTab tab ResignPressed)
      | (tab, _) <- maybe [] pure (shownGame state)
      ]
    )
  ]

-- | Where a tab stands, which is the window's title.
statusOf :: Stage -> Text
statusOf (Choosing _      ) = "New game"
statusOf (Playing session) = stateOf session

-- | The line under the title.
detailOf :: Stage -> Text
detailOf (Choosing setup  ) = describeSetup setup
detailOf (Playing session) = aboutGame session

-- | Where a game stands.
stateOf :: Session -> Text
stateOf session = case session.opponent of
  Starting -> "Starting\8230"
  Gone _   -> "Stopped"
  _ | finished game    -> ending session
    | playable session -> "Your move"
    | otherwise        -> "Thinking\8230"
  where game = session.game

-- | How a game that is over ended.
ending :: Session -> Text
ending session = case winnerByResignation session.game of
  Just winner | winner == session.human -> "The engine resigned"
              | otherwise                      -> "You resigned"
  Nothing                                      -> "Game over"

-- | The line under the title, while a game is going on.
--
-- Ordinarily the numbers, which is what somebody looks down at while
-- they play. When the game has something to say instead, it says it
-- there: an engine that is gone, a score at the end, or a point the
-- rules would not take a stone on.
aboutGame :: Session -> Text
aboutGame session = case session.opponent of
  Gone why -> why
  _        -> case session.note of
    Just (Result  out ) -> "Result: " <> out
    Just (Refused what) -> describeIllegal what
    Nothing             -> capturesOf session

-- | The board, and how many stones each player has taken.
capturesOf :: Session -> Text
capturesOf session =
  size'
    <> "  \183  Black "
    <> took Black
    <> "  \183  White "
    <> took White
 where
  captures = session.game.captures
  size' = let n = Text.pack (show (sessionSize session)) in n <> "\215" <> n
  took side = Text.pack (show (capturedBy side captures))

-- | The games, one to a tab.
gameTabs :: State -> Widget Event
gameTabs state = tabView
  [#name := viewName]
  defaultTabViewParams
    { tabs        = Vector.fromList (map tabFor state.openTabs)
    , selected    = keyOf <$> state.showing
    , onSelected  = Just TabSelected
    , onClosePage = Just TabClosePressed
    , onReordered = Just TabsReordered
    }
 where
  tabFor (tab, Playing session) = Tab
    { tabKey   = keyOf tab
    , tabTitle = "Game " <> Text.pack (show tab)
    , tabChild = board tab session
    }
  tabFor (tab, Choosing setup) = Tab
    { tabKey   = keyOf tab
    , tabTitle = "New Game"
    , tabChild = InSetup tab <$> launchPage setup
    }

-- | One game's board, with a little room around it so that the wood
-- does not touch the edge of the window.
board :: TabId -> Session -> Widget Event
board tab session = toEvent <$> goban
  [ #marginStart := 8
  , #marginEnd := 8
  , #marginTop := 8
  , #marginBottom := 8
  ]
  props
 where
  game  = session.game
  props = GobanProps
    { board       = game.board
    , last        = game.last
    , hover       = session.human <$ ready session
    , coordinates = True
    }
  toEvent (GobanClicked coord) = InTab tab (Clicked coord)

-- | The window, ready to run.
application
  :: Opponents -> Setup -> App Adw.ApplicationWindow State Event
application opponents opening = defaultApp
  { update       = update'
  , view         = view'
  , initialState = startingState opponents opening
    -- The one event the window sends itself, which puts the first tab
    -- up. That tab opens on a page and starts nothing.
  , inputs       = [Pipes.yield NewTabPressed]
  }
