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
  , startingState
  , Doing(..)
  , decide
  , update'
  , view'
  , application
    -- * The lines the header bar shows
  , statusOf
  , detailOf
  , capturesOf
  )
where

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

-- | Tells one tab from another, for as long as the window is open. A
-- number rather than the tab's place in the list, because tabs are
-- closed and made and dragged about, and a name that moves is a name
-- that sends an answer to the wrong game.
type TabId = Int

-- | Everything the window shows.
data State = State
  { stateGames     :: [(TabId, Session)]
    -- ^ The open games, in the order their tabs are in.
  , stateShowing   :: Maybe TabId
    -- ^ The tab whose game the bars at the top and the bottom are
    -- about.
  , stateNextTab   :: TabId
    -- ^ The name the next tab gets.
  , stateHuman     :: Color
    -- ^ The colour the player takes in a new game.
  , stateOpponents :: Opponents
    -- ^ Where a new tab gets its opponent.
  }

-- | What the window reports.
data Event
  = InTab TabId SessionEvent
    -- ^ Something happened to the game in this tab.
  | TabOpened TabId (Either Text Engine)
    -- ^ The opponent a new tab was waiting for, or why there is none.
  | NewTabPressed Int
    -- ^ Open a game on a board this wide, in a tab of its own.
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
startingState :: Opponents -> Color -> State
startingState opponents human = State { stateGames     = []
                                      , stateShowing   = Nothing
                                      , stateNextTab   = 1
                                      , stateHuman     = human
                                      , stateOpponents = opponents
                                      }

-- * Updating
-------------

-- | What an event does to the window: where it leaves it, and what it
-- asks somebody to do, if anything.
--
-- The doing is an action rather than a thread that has been started,
-- so that deciding stays a function of its arguments. 'update'' is
-- this with the action handed to the application to run, which is the
-- only part that needs an application to be running.
data Doing
  = Carry State (Maybe (IO (Maybe Event)))
    -- ^ The window carries on from here, and this is what to do.
  | Close
    -- ^ The window ends.

update' :: State -> Event -> Transition State Event
update' state event = case decide state event of
  Close            -> Exit
  -- The job is not named, so nothing stops it part way through. A
  -- command is several lines of protocol, and a job stopped between
  -- two of them would leave the answer to the first sitting in the
  -- pipe, to be read as the answer to whatever was asked next.
  Carry state' job -> Transition state' (maybe none perform job)

-- | What one event does to the window.
decide :: State -> Event -> Doing
decide state = \case
  Closed                       -> Close

  NewTabPressed n              -> openTab state n

  TabOpened tab (Right engine) -> inTab state tab (opened engine)
  TabOpened tab (Left  why   ) -> inTab state tab (stayAt . couldNotOpen why)

  InTab tab event              -> inTab state tab (`step` event)

  TabSelected key              -> carry state { stateShowing = tabOf state key }

  TabClosePressed key          -> closeTab state key

  TabsReordered keys           -> carry state { stateGames = inOrder }
    where inOrder = mapMaybe (withId state) (Vector.toList keys)

-- | The window carries on from here, with nothing to do.
carry :: State -> Doing
carry state = Carry state Nothing

-- | A step that changes the game and asks nobody anything.
stayAt :: Session -> Step
stayAt session = Step session Nothing

-- | Do something to the game in one tab.
--
-- An event for a tab that is not there is dropped. That is how an
-- answer from an opponent whose tab was closed while it was thinking
-- ends: the opponent was stopped, the answer it was in the middle of
-- arrives as a failure, and there is no longer a game it is about.
inTab :: State -> TabId -> (Session -> Step) -> Doing
inTab state tab move = case lookup tab (stateGames state) of
  Nothing      -> carry state
  Just session -> Carry state { stateGames = replaced } job
   where
    Step session' asked = move session
    replaced =
      [ (tab', if tab' == tab then session' else other)
      | (tab', other) <- stateGames state
      ]
    job = fmap (fmap (Just . InTab tab . Answered)) asked

-- | Open a game on a board this wide, in a tab of its own.
openTab :: State -> Int -> Doing
openTab state n = Carry
  state { stateGames   = stateGames state <> [(tab, starting human n)]
        , stateShowing = Just tab
        , stateNextTab = tab + 1
        }
  (Just (Just . TabOpened tab <$> openOpponent (stateOpponents state) n))
 where
  tab   = stateNextTab state
  human = stateHuman state

-- | Close a tab, and let its opponent go.
--
-- The window closes with its last tab. A window with no games in it
-- would have nothing to show and nothing to do, and a new game is a
-- new window away. The opponent of that last tab is not let go here,
-- because an ending window does nothing else afterwards: what stops it
-- is the same thing that stops the opponents of any tabs still open,
-- which is whatever handed this window its 'Opponents'.
closeTab :: State -> Text -> Doing
closeTab state key = case withId state key of
  Nothing             -> carry state
  Just (tab, session) -> case remaining of
    [] -> Close
    _  -> Carry state { stateGames = remaining, stateShowing = showing' }
                (release (stateOpponents state) session)
   where
    remaining = [ open | open <- stateGames state, fst open /= tab ]
    -- Showing the tab that took the closed one's place, or the last
    -- one, which is what a tab bar does.
    showing' = case stateShowing state of
      Just showed | showed /= tab -> Just showed
      _                           -> fst <$> nextAfter tab (stateGames state)

-- | Stop the opponent of a game whose tab has closed.
--
-- An opponent that is thinking is stopped in the middle of it. That is
-- what stopping is for, and the answer it was about to give arrives at
-- a tab that is no longer there, where it is dropped.
release :: Opponents -> Session -> Maybe (IO (Maybe Event))
release opponents session = case sessionOpponent session of
  Idle    engine -> Just (letGo engine)
  Waiting engine -> Just (letGo engine)
  Starting       -> Nothing
  Gone _         -> Nothing
  where letGo engine = Nothing <$ closeOpponent opponents engine

-- | The tab after this one, or the one before it when this is the
-- last.
nextAfter :: TabId -> [(TabId, Session)] -> Maybe (TabId, Session)
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

-- | The tab a key names, and the game in it.
withId :: State -> Text -> Maybe (TabId, Session)
withId state key = find ((== key) . keyOf . fst) (stateGames state)

-- | The tab the bars at the top and the bottom are about, and the game
-- in it.
showing :: State -> Maybe (TabId, Session)
showing state = do
  tab     <- stateShowing state
  session <- lookup tab (stateGames state)
  pure (tab, session)

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
        , toolbarContent (games state)
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
        [ #title := maybe "Stones" (statusOf . snd) (showing state)
        , #subtitle := maybe "" (detailOf . snd) (showing state)
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
-- showing. A window with no game showing has the buttons, greyed.
action
  :: State -> Text -> Text -> (Session -> Bool) -> SessionEvent -> Widget Event
action state icon tip enabled event = case showing state of
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
  [ menuSection
    Nothing
    [ subMenu
        "New Game"
        [ menuItem "9\215\&9"   (NewTabPressed 9)
        , menuItem "13\215\&13" (NewTabPressed 13)
        , menuItem "19\215\&19" (NewTabPressed 19)
        ]
    ]
  , menuSection
    Nothing
    (Vector.fromList
      [ menuItem "Resign" (InTab tab ResignPressed)
      | (tab, _) <- maybe [] pure (showing state)
      ]
    )
  ]

-- | Where the game stands, which is the window's title.
statusOf :: Session -> Text
statusOf session = case sessionOpponent session of
  Starting -> "Starting\8230"
  Gone _   -> "Stopped"
  _ | finished game    -> ending session
    | playable session -> "Your move"
    | otherwise        -> "Thinking\8230"
  where game = sessionGame session

-- | How a game that is over ended.
ending :: Session -> Text
ending session = case winnerByResignation (sessionGame session) of
  Just winner | winner == sessionHuman session -> "The engine resigned"
              | otherwise                      -> "You resigned"
  Nothing                                      -> "Game over"

-- | The line under the title.
--
-- Ordinarily the numbers, which is what somebody looks down at while
-- they play. When the game has something to say instead, it says it
-- there: an engine that is gone, a score at the end, or a point the
-- rules would not take a stone on.
detailOf :: Session -> Text
detailOf session = case sessionOpponent session of
  Gone why -> why
  _        -> case sessionNote session of
    Just (Result  out ) -> "Result: " <> out
    Just (Refused what) -> describeIllegal what
    Nothing             -> capturesOf session

-- | The board, and how many stones each player has taken.
capturesOf :: Session -> Text
capturesOf session =
  size'
    <> "  \183  Black "
    <> took blackCaptured
    <> "  \183  White "
    <> took whiteCaptured
 where
  captures = gameCaptures (sessionGame session)
  size' = let n = Text.pack (show (sessionSize session)) in n <> "\215" <> n
  took field = Text.pack (show (field captures))

-- | The games, one to a tab.
games :: State -> Widget Event
games state = tabView
  [#name := viewName]
  defaultTabViewParams
    { tabs        = Vector.fromList (map tabFor (stateGames state))
    , selected    = keyOf <$> stateShowing state
    , onSelected  = Just TabSelected
    , onClosePage = Just TabClosePressed
    , onReordered = Just TabsReordered
    }
 where
  tabFor (tab, session) = Tab
    { tabKey   = keyOf tab
    , tabTitle = "Game " <> Text.pack (show tab)
    , tabChild = board tab session
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
  game  = sessionGame session
  props = GobanProps
    { gobanBoard       = gameBoard game
    , gobanLast        = gameLast game
    , gobanHover       = sessionHuman session <$ ready session
    , gobanCoordinates = True
    }
  toEvent (GobanClicked coord) = InTab tab (Clicked coord)

-- | The window, ready to run.
application
  :: Opponents -> Color -> Int -> App Adw.ApplicationWindow State Event
application opponents human n = defaultApp
  { update       = update'
  , view         = view'
  , initialState = startingState opponents human
    -- The one event the window sends itself, which opens the first
    -- game.
  , inputs       = [Pipes.yield (NewTabPressed n)]
  }
