-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE NoFieldSelectors      #-}

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
  , Aside(..)
  , asidesKept
  , startingState
  , update'
  , listeners
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

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( forever )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Bifunctor                 ( bimap )
import           Data.List                      ( find )
import           Data.Maybe                     ( isJust
                                                , mapMaybe
                                                )
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
import           GI.Gtk.Declarative.Adwaita.ToastOverlay
                                                ( Toast(..)
                                                , ToastOverlayParams(..)
                                                , defaultToastOverlayParams
                                                , toast
                                                , toastOverlay
                                                )
import           GI.Gtk.Declarative.Adwaita.ToolbarView
                                                ( toolbarContent
                                                , toolbarTop
                                                )
import           GI.Gtk.Declarative.App.Simple
import           Pipes                          ( Producer )
import qualified Pipes

import           Go.Game
import           Go.Types
import           Stones.Opponent
import qualified Stones.Menu                   as Menu
import           Stones.Chat                    ( chatPane )
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
  , already   :: [Ongoing]
    -- ^ The games this player is already in that this window has no
    -- tab for. Asked for once they are signed in, and again whenever
    -- they ask.
  , offers    :: [Invitation]
    -- ^ Games somebody has offered and nobody has answered yet.
  , asides    :: [Aside]
    -- ^ Things said about games the player is not looking at, waiting
    -- to be shown over whatever they are looking at.
  , showChat  :: Bool
    -- ^ Whether the pane beside the board is out. One flag for the
    -- window rather than one per tab: it is a way of using the
    -- program, and a player who wants to see what is said wants to see
    -- it in whichever game they are looking at.
  , signing   :: Signing
    -- ^ How far the player has got with signing in, which one of the
    -- two places asks for. It belongs to the window rather than to a
    -- tab: signing in once is signing in for all of them.
  }

-- | Something said about a game the player is not looking at.
--
-- A message that arrived in a tab behind this one, or in this one with
-- the pane shut, is a message nobody sees. This is that message on its
-- way to being shown over the board for a few seconds.
data Aside = Aside
  { key  :: Text
    -- ^ What tells this one from the others. The overlay shows a name
    -- it has not shown, and leaves one it has alone until it goes.
  , line :: Text
  }
  deriving (Eq, Show)

-- | How many are kept.
--
-- One is taken off the list when the overlay says it has gone. A
-- handful is as many as anybody reads anyway, and keeping no more than
-- that means a window whose overlay never says so does not grow a
-- list forever.
asidesKept :: Int
asidesKept = 3

-- | What the window reports.
data Event
  = InTab TabId SessionEvent
    -- ^ Something happened to the game in this tab.
  | InSetup TabId SetupEvent
    -- ^ Somebody chose something on this tab's page, or started it.
  | TabOpened TabId (Either Text Opponent)
    -- ^ The opponent a game was waiting for, or why there is none.
  | NewTabPressed
    -- ^ Open a tab, on the page that asks what to play.
  | TabSelected Text
  | TabClosePressed Text
  | TabsReordered (Vector Text)
  | ResignChosen
    -- ^ Give up the game showing, which is what the menu means by it.
  | PlayAgainChosen
    -- ^ Start the game showing over, which is what the menu means by
    -- it. Which tab that is is worked out when this arrives rather
    -- than when the menu was drawn, the same way giving up is.
  | AgainPressed TabId
    -- ^ Start this tab's game over, with what it was started from.
  | GamesFound (Either Text [Ongoing])
    -- ^ The games this player is already in, or why they could not be
    -- asked for.
  | OfferTaken Invitation
    -- ^ Take a game somebody offered, in a tab of its own.
  | AsideGone Text
    -- ^ Something said over the board has left the screen.
  | ChatToggled Bool
    -- ^ Show or hide the pane beside the board.
  | SignedInAs (Either Text Text)
    -- ^ The signing in that was started finished, either with what to
    -- call the player or with why not.
  | Ticking
    -- ^ A second has gone by, and every clock that is going down goes
    -- down by one.
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
                                        , already   = []
                                        , offers    = []
                                        , asides    = []
                                        , showChat  = False
                                        , signing   = SignedOut
                                        }

-- * Updating
-------------

update' :: State -> Event -> Transition State Event
update' state = \case
  Closed                       -> Exit

  NewTabPressed                -> openTab state

  -- These are about the window rather than about the tab the button
  -- happens to be on, so they never reach the page.
  InSetup _ SignInPressed      -> signIn state

  InSetup _ RefreshPressed     -> Transition state (askingForGames state)

  InSetup _ (ResumePressed it) -> goingBackTo state it

  InSetup tab event            -> inSetup state tab event

  ChatToggled out              -> Transition state { showChat = out } none

  -- Signed in, and now there is somebody to have games. What they are
  -- already in is worth knowing before they are asked what to start.
  SignedInAs (Right name)      -> Transition state { signing = SignedIn name }
                                             (askingForGames state)

  -- A list that could not be asked for is left as it was. It is a list
  -- of what is true somewhere else, and not being able to see it is
  -- not the same as there being nothing in it.
  GamesFound (Right found)     -> Transition state { already = found } none
  GamesFound (Left  _    )     -> Transition state none
  SignedInAs (Left  why )      ->
    Transition state { signing = SignInRefused why } none

  TabOpened tab (Right them)   -> case lookup tab state.openTabs of
    Just (Playing _) -> inGame state tab (opened them)
    -- The tab closed while its opponent was being found. Nobody here
    -- is going to play this game, so it is given back rather than left
    -- running: on a server, a game left running is somebody else
    -- sitting at a board waiting for a player who will never arrive.
    _                -> Transition state (perform (Nothing <$ state.opponents.close them))
  TabOpened tab (Left  why   ) -> inGame state tab (stayAt . couldNotOpen why)

  -- An offer is not about the game it arrives on. A connection carries
  -- everything said to this player, and a game somebody is offering is
  -- one of those things, so the window takes it rather than the game.
  InTab _ (Arrived (Invited offer)) -> Transition
    state { offers = keepingLast asidesKept (state.offers <> [offer]) }
    none

  InTab tab event              -> inGame (noticing state tab event)
                                         tab
                                         (`step` event)

  OfferTaken offer             -> taking state offer

  AgainPressed tab             -> case lookup tab state.openTabs of
    Just (Playing session) | canPlayAgain session -> again state tab session
    _                                             -> Transition state none

  AsideGone gone               -> Transition
    state { asides = [ aside | aside <- state.asides, aside.key /= gone ] }
    none

  TabSelected key              -> Transition state { showing = tabOf state key } none

  TabClosePressed key          -> closeTab state key

  TabsReordered keys           -> Transition state { openTabs = inOrder } none
    where inOrder = mapMaybe (withId state) (Vector.toList keys)

  -- Every game at once, rather than one stream per tab. A second is
  -- the same second in all of them.
  Ticking                      -> Transition state { openTabs = counted } none
    where
      counted = [ (tab, onward held) | (tab, held) <- state.openTabs ]
      onward (Playing session) = Playing (tick session)
      onward held              = held

  -- The menu acts on whatever is showing, which is what a menu does.
  -- The item is greyed when there is no game to give up, so this only
  -- arrives when there is one.
  ResignChosen                 -> case shownGame state of
    Just (tab, _) -> inGame state tab (`step` ResignPressed)
    Nothing       -> Transition state none

  PlayAgainChosen              -> case shownGame state of
    Just (tab, session) | canPlayAgain session -> again state tab session
    _                                          -> Transition state none

-- | Ask which games this player is already in.
askingForGames :: State -> Cmd Event
askingForGames state =
  keyed "games" (perform (Just . GamesFound <$> state.opponents.ongoing))

-- | Open a game this player is already in, in a tab of its own.
--
-- It is taken off the list as it opens, so that the page stops
-- offering a game that now has a tab. Closing that tab does not put it
-- back, because looking again is a button.
goingBackTo :: State -> Ongoing -> Transition State Event
goingBackTo state it = Transition
  state { openTabs = state.openTabs <> [(tab, Playing (starting it.about))]
        , showing  = Just tab
        , nextTab  = tab + 1
        , already  = [ other
                     | other <- state.already
                     , other.number /= it.number
                     ]
        }
  (keyed (findingIn tab)
         (perform (Just . TabOpened tab <$> state.opponents.resume it))
  )
  where tab = state.nextTab

-- | Take a game somebody offered, in a tab of its own.
--
-- A tab of its own because the game that is running is still running:
-- an offer arrives while somebody is playing, and most of the time the
-- one it follows is the one that just ended in the tab it came in on.
--
-- The offer is taken off the list whether it works or not. Saying yes
-- twice to one offer is saying yes to a game that already started.
taking :: State -> Invitation -> Transition State Event
taking state offer = Transition
  state { openTabs = state.openTabs <> [(tab, Playing (starting offer.about))]
        , showing  = Just tab
        , nextTab  = tab + 1
        , offers   = [ other | other <- state.offers, other.token /= offer.token ]
        }
  (keyed (findingIn tab)
         (perform (Just . TabOpened tab <$> state.opponents.take' offer))
  )
  where tab = state.nextTab

-- | Start a tab's game over, with what it was started from rather than
-- with whatever the page is asking for now.
--
-- The opponent that was playing is let go, because the game it was
-- playing is over as far as this window is concerned, and on a server
-- an opponent left running is a board somebody else is still sitting
-- at. Then the same thing is asked for again: another engine on this
-- machine, or another game on the server, which is what a rematch is.
again :: State -> TabId -> Session -> Transition State Event
again state tab session = Transition
  (replacing state tab (Playing (starting wanted)))
  (  release state.opponents (Playing session)
  <> keyed (findingIn tab)
           (perform (Just . TabOpened tab <$> state.opponents.open wanted))
  )
  where wanted = playingAgain session

-- | Something said about a game the player is not looking at goes
-- where they will see it.
--
-- Not what is said about the game in front of them with the pane open.
-- They are looking at it.
noticing :: State -> TabId -> SessionEvent -> State
noticing state tab = \case
  Arrived (Spoke who what) | not (watching state tab) -> state
    { asides = lastFew (state.asides <> [Aside (asideIn state tab) (who <> ": " <> what)])
    }
  _ -> state
 where
  lastFew asides = keepingLast asidesKept asides

-- | The last few of a list, and all of it when there are no more than
-- that.
keepingLast :: Int -> [a] -> [a]
keepingLast many held = drop (length held - many) held

-- | Is the player looking at what is said about this game?
watching :: State -> TabId -> Bool
watching state tab = state.showing == Just tab && state.showChat

-- | A name for the next thing said about a game.
--
-- How much has been said about it already, which no two messages in
-- one game share, and the tab, which no two games share.
asideIn :: State -> TabId -> Text
asideIn state tab = "said " <> keyOf tab <> " " <> Text.pack (show sofar)
 where
  sofar = case lookup tab state.openTabs of
    Just (Playing session) -> length session.said
    _                      -> 0

-- | Start signing in, which opens a browser and waits.
--
-- Pressing it twice while it is going is not two sign-ins. The first
-- one has a browser window open somewhere and is waiting on it, and a
-- second would open another and wait on a port the first one has.
signIn :: State -> Transition State Event
signIn state
  | state.signing == SigningIn = Transition state none
  | otherwise = Transition
    state { signing = SigningIn }
    (perform (Just . SignedInAs <$> state.opponents.enter))

-- | A game that has changed without anything being asked of anybody.
stayAt :: Session -> Step
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
      (replacing state tab (Playing (starting setup)))
        -- What was started is what the next tab asks for first.
        { opening = setup
        }
      -- Under the tab's own name, so that closing the tab can stop it.
      -- Finding an opponent on a server is waiting for somebody else
      -- to want the same game, which can take minutes, and a tab that
      -- closes while it waits should stop waiting.
      (keyed (findingIn tab)
             (perform (Just . TabOpened tab <$> state.opponents.open setup))
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
inGame :: State -> TabId -> (Session -> Step) -> Transition State Event
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
                     (calledOff tab held <> release state.opponents held)
   where
    remaining = [ open | open <- state.openTabs, fst open /= tab ]
    -- Showing the tab that took the closed one's place, or the last
    -- one, which is what a tab bar does.
    showing' = case state.showing of
      Just showed | showed /= tab -> Just showed
      _                           -> fst <$> nextAfter tab state.openTabs

-- | Stop looking for an opponent for a tab that has closed.
--
-- A job under a name stops whatever is running under that name, so a
-- job with nothing in it is how a search is called off.
--
-- Only a game that is still waiting for one has anything to call off.
-- A tab that is still a page never started a search, and a game that
-- has an opponent, or has given up on one, is long past it.
calledOff :: TabId -> Stage -> Cmd Event
calledOff tab (Playing session) = case session.standing of
  Starting -> keyed (findingIn tab) (perform (pure Nothing))
  _        -> none
calledOff _ _ = none

-- | The name the search for one tab's opponent runs under.
findingIn :: TabId -> Text
findingIn tab = "finding " <> keyOf tab

-- | Stop the opponent of a tab that has closed.
--
-- A tab that is still a page has none. An opponent that is thinking is
-- stopped in the middle of it. That is what stopping is for, and the
-- answer it was about to give arrives at a tab that is no longer
-- there, where it is dropped.
release :: Opponents -> Stage -> Cmd Event
release _         (Choosing _      ) = none
release opponents (Playing session) = case listening session of
  Just them -> perform (Nothing <$ opponents.close them)
  Nothing   -> none

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

-- | What the window listens to, which is one stream for each game
-- that has an opponent.
--
-- The name carries the tab, so two games are two subscriptions. That
-- is what 'GI.Gtk.Declarative.App.Simple.qualifyingSubs' is for where
-- the names come from somewhere else, and it is not needed here
-- because these names are made in one place and the tab is in them
-- already.
--
-- A tab that closes takes its name out, and the loop cancels the
-- stream. A game whose opponent has gone does the same.
listeners :: State -> [Sub Event]
listeners state =
  [ InTab tab . Arrived <$> sub (heardFrom tab) them.hear
    | (tab, Playing session) <- state.openTabs
    , Just them <- [listening session]
    ]
    <> [ Ticking <$ sub "clock" seconds | any ticked state.openTabs ]
 where
  ticked (_, Playing session) = counting session
  ticked _                    = False

-- | A second, over and over.
--
-- One stream for the whole window rather than one per game, because a
-- second is the same second in every tab, and because a window with no
-- clock anywhere in it should not be woken at all. It starts when the
-- first clock starts and stops when the last one does: the loop keeps
-- a stream for as long as its name is in the list, and this name is
-- there only while something is being timed.
seconds :: Producer () IO ()
seconds = forever (liftIO (threadDelay 1000000) >> Pipes.yield ())

-- | The name the stream of one tab runs under.
heardFrom :: TabId -> Text
heardFrom tab = "heard " <> keyOf tab

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
    $ toastOverlay
        []
        (defaultToastOverlayParams bars)
          { toasts = Vector.fromList (map saidOver state.asides) }
 where
  bars = container
    Adw.ToolbarView
    []
    [ toolbarTop (header state)
    , toolbarTop (widget Adw.TabBar [tabBarView viewName])
    , toolbarContent (gameTabs state)
    ]

-- | Something said about a game the player is not looking at, shown
-- over the board for a few seconds.
--
-- It is taken off the window's list when it leaves the screen, so that
-- the same words said twice are two of these rather than one the
-- overlay has already shown and will not show again.
saidOver :: Aside -> Toast Event
saidOver aside = (toast aside.key aside.line)
  { onDismissed = Just (AsideGone aside.key)
  }

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
    (action state "edit-undo-symbolic" "Undo" canUndo (`InTab` UndoPressed))
  -- Starting the game over is in the menu rather than here. It was a
  -- button next to this one, and a player reaching for Undo by
  -- position found the one that throws the whole game away.
  -- Packed from the right edge inward, so the menu is the button in
  -- the corner, which is where a GNOME program keeps it.
  , headerBarEnd (mainMenu state)
  , headerBarEnd (finishing state)
  , headerBarEnd (talking state)
  ]

-- | The button that shows the pane beside the board.
--
-- Only a game with somebody on the other side has one at all. A
-- program takes what is said to it and says nothing back, and a button
-- that opened an empty pane beside a board where nothing would ever
-- answer is worse than no button.
talking :: State -> Widget Event
talking state
  | maybe False (canTalk . snd) (shownGame state) = widget
    Gtk.ToggleButton
    [ #iconName := "chat-message-new-symbolic"
    , #tooltipText := "Show what has been said about this game"
    , #active := state.showChat
    , on #toggled (ChatToggled (not state.showChat))
    ]
  | otherwise = widget
    Gtk.Box
    [#visible := False, #widthRequest := 0]

-- | The button at the right-hand end.
--
-- Pass while the game is being played, and Accept once it has
-- stopped. They are the same button because they are the same thing
-- at two moments: the one that hands the game on. Two buttons, with
-- whichever does not apply greyed, would leave a player looking for
-- the difference between them.
finishing :: State -> Widget Event
finishing state
  | maybe False (settling . snd) (shownGame state) = action
    state
    "object-select-symbolic"
    "Accept the stones marked dead and count the game"
    canAccept
    (`InTab` AcceptPressed)
  | otherwise = action state
                       "media-skip-forward-symbolic"
                       "Pass, giving the move to the other player"
                       playable
                       (`InTab` Passed)

-- | A button in the header bar that does something to the game
-- showing.
--
-- A tab that is still a page has no game to do it to, and neither has
-- a window with no tabs, so both have the buttons and both have them
-- greyed.
action
  :: State
  -> Text
  -> Text
  -> (Session -> Bool)
  -> (TabId -> Event)
  -> Widget Event
action state icon tip enabled event = case shownGame state of
  Nothing -> widget
    Gtk.Button
    [#iconName := icon, #tooltipText := tip, #sensitive := False]
  Just (tab, session) -> widget
    Gtk.Button
    [ #iconName := icon
    , #tooltipText := tip
    , #sensitive := enabled session
    , on #clicked (event tab)
    ]

-- | The menu in the corner.
--
-- The menu itself is markup, in @data\/ui\/menu.blp@. What is chosen
-- from it arrives here as one of two events.
mainMenu :: State -> Widget Event
mainMenu state = chosen <$> Menu.mainMenu
  [ #iconName := "open-menu-symbolic"
  , #primary := True
  , #tooltipText := "Main Menu"
  ]
  Menu.MenuProps
    { canPlayAgain = maybe False (canPlayAgain . snd) (shownGame state)
    , canResign    = isJust (shownGame state)
    }
 where
  chosen Menu.NewGameChosen   = NewTabPressed
  chosen Menu.PlayAgainChosen = PlayAgainChosen
  chosen Menu.ResignChosen    = ResignChosen

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
stateOf session = case session.standing of
  Starting -> "Starting\8230"
  Gone _   -> "Stopped"
  -- Before the line below, because a game being counted has finished
  -- as far as the rules here go: both players passed, and what is left
  -- is not a move.
  _ | settling session -> "Count the game"
    | finished game    -> ending session
    | playable session -> "Your move"
    | otherwise        -> "Thinking\8230"
  where game = session.game

-- | How a game that is over ended.
ending :: Session -> Text
ending session = case winnerByResignation session.game of
  Just winner | winner == session.human -> opponentOf session <> " resigned"
              | otherwise               -> "You resigned"
  Nothing                               -> "Game over"

-- | The line under the title, while a game is going on.
--
-- Ordinarily the numbers, which is what somebody looks down at while
-- they play. When the game has something to say instead, it says it
-- there: an engine that is gone, a score at the end, or a point the
-- rules would not take a stone on.
aboutGame :: Session -> Text
aboutGame session = case session.standing of
  Gone why -> why
  -- Who the game is against goes here as well as on the tab, because
  -- a window with one game in it has no tabs: the bar hides itself
  -- rather than showing a row of one.
  Here it  -> it.name <> "  \183  " <> saying
  _        -> saying
 where
  saying
    -- While the game is being counted the numbers are not the thing to
    -- say. Nothing about counting is written on the board, so what the
    -- player is meant to do with it has to be.
    | settling session = "Click a group to mark it dead, then accept"
    | otherwise = case session.note of
      Just (Result  out ) -> "Result: " <> out
      Just (Refused what) -> describeIllegal what
      Nothing             -> capturesOf session

-- | The board, how many stones each player has taken, and, in a game
-- that is timed, how long each has left.
--
-- Always in the same order, Black and then White, rather than whoever
-- is to play first. The title above already says whose turn it is, and
-- a line whose halves swap over every move is a line nobody can read
-- at a glance.
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
  took side =
    Text.pack (show (capturedBy side captures)) <> maybe "" left (clockOf session side)
  left time = "  " <> describeTime time

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
      -- Who the game is against, which is the one thing that tells two
      -- tabs apart at a glance. A number never did: the tabs are in a
      -- row and already look like one, two, three.
    , tabTitle = titleOf session
    , tabChild = heldInATab (board state tab session)
    }
  tabFor (tab, Choosing setup) = Tab
    { tabKey   = keyOf tab
    , tabTitle = "New Game"
    , tabChild = heldInATab
                   (InSetup tab <$> launchPage state.signing state.already setup)
    }

-- | What a tab holds, under a wrapper that never changes shape.
--
-- A page holds the widget it was made with, so a widget that has to be
-- replaced rather than patched is a page that has to be made again,
-- and a page made again is not the page that was selected: the view
-- falls back to a neighbour and the tab somebody was looking at is not
-- the tab they end up on.
--
-- What a tab holds does change shape. A page that asks what to play
-- becomes a board, and a board grows a pane beside it the moment there
-- is somebody to talk to. So the thing the page is given is a box that
-- is always the same box, and everything that changes happens inside
-- it, where a replacement is an ordinary one.
heldInATab :: Widget Event -> Widget Event
heldInATab inside = container
  Gtk.Box
  [#orientation := Gtk.OrientationVertical]
  [BoxChild defaultBoxChildProperties { expand = True, fill = True } inside]

-- | What a tab holding a game is called.
--
-- A game that has not found anybody yet has nobody to be named after,
-- and one whose opponent never arrived is named after what went wrong
-- somewhere else, not here.
titleOf :: Session -> Text
titleOf session = case session.standing of
  Here it -> it.name <> left
  _       -> "Game"
 where
  -- How long this player has left in this game, which is the thing
  -- worth glancing across a row of tabs for: several games at once and
  -- one of them about to run out. Not once it has ended, because the
  -- time left in a game nobody is playing is not news.
  left
    | finished session.game = ""
    | otherwise = maybe
      ""
      (("  \183  " <>) . describeTime)
      (clockOf session session.human)

-- | One game's board, with a little room around it so that the wood
-- does not touch the edge of the window.
board :: State -> TabId -> Session -> Widget Event
board state tab session
  | canTalk session = container
    Gtk.Box
    [#orientation := Gtk.OrientationHorizontal]
    [ BoxChild defaultBoxChildProperties { expand = True, fill = True }
      (onlyTheBoard tab session)
    , BoxChild defaultBoxChildProperties $ bin
      Gtk.Revealer
      [ #revealChild := state.showChat
      , #transitionType := Gtk.RevealerTransitionTypeSlideLeft
      ]
      (InTab tab <$> chatPane session.said)
    ]
  | otherwise = onlyTheBoard tab session

-- | The board on its own, which is the whole of a tab in a game
-- nobody can be talked to in.
onlyTheBoard :: TabId -> Session -> Widget Event
onlyTheBoard tab session = toEvent <$> goban
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
    , dead        = deadIn session
    , marking     = settling session
      -- Not while it is being counted: there is still something to do
      -- on it then, and a board gone quiet says there is not.
    , over        = finished game && not (settling session)
    }
  toEvent (GobanClicked coord) = InTab tab (Clicked coord)

-- | The window, ready to run.
application
  :: Opponents -> Setup -> App Adw.ApplicationWindow State Event
application opponents opening = defaultApp
  { update        = update'
  , view          = view'
  , subscriptions = listeners
  , initialState  = startingState opponents opening
    -- The one event the window sends itself, which puts the first tab
    -- up. That tab opens on a page and starts nothing.
  , inputs        = [Pipes.yield NewTabPressed]
  }
