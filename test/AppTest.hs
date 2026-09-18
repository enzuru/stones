-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The tabs: which games are open, which one is showing, and where
-- each answer belongs.
module AppTest
  ( tests
  )
where

import           Data.Foldable                  ( traverse_ )
import           Data.IORef
import           Data.Text                      ( Text )
import           Hedgehog
import qualified Pipes.Prelude                 as Pipes
import qualified Data.Vector                   as Vector

import           GI.Gtk.Declarative.App.Simple  ( App(..)
                                                , Cmd
                                                , Transition(..)
                                                , jobsOf
                                                , none
                                                )

import           Go.Game
import           Go.Types
import           Stones.App
import           Stones.Engine
import           Stones.Session
import           Stones.Setup

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

-- | A source of opponents that hands out the same silent one.
source :: Opponents
source = Opponents { open  = \_ _ -> pure (Right silent)
                   , close = \_ -> pure ()
                   }

-- | A source that writes down what it hands out and what it is given
-- back, which is how the tests below see that a tab closing stops the
-- program that was playing in it.
countingSource :: IO (Opponents, IORef (Int, Int))
countingSource = do
  counts <- newIORef (0, 0)
  let handedOut  = modifyIORef' counts (\(out, back) -> (out + 1, back))
      handedBack = modifyIORef' counts (\(out, back) -> (out, back + 1))
  pure
    ( Opponents { open  = \_ _ -> Right silent <$ handedOut
                , close = \_ -> handedBack
                }
    , counts
    )

-- | Run what an event asked somebody to do, and answer with the events
-- it came back with.
--
-- The jobs run under no name, here and in a game, because nothing this
-- window asks for can be stopped part way through. A window that named
-- them would want 'qualifying' as well, since a name is shared by
-- everything the loop runs and this window holds a game per tab.
ran :: State -> Event -> PropertyT IO [Event]
ran state event = case jobsOf (commandOf (update' state event)) of
  []    -> annotate "nothing was asked" >> failure
  named -> do
    map fst named === map (const Nothing) named
    concat <$> traverse (evalIO . Pipes.toListM . snd) named

-- | The command a transition answers with.
commandOf :: Transition state event -> Cmd event
commandOf (Transition _ cmd) = cmd
commandOf Exit               = none

-- | A window with nothing open in it, whose page asks for a 9x9 game
-- as Black.
empty' :: State
empty' = startingState source asking

-- | What the page of a new tab asks for, in these tests.
asking :: Setup
asking = Setup { size = 9, human = Black, strength = Fierce }

-- | The tab that is open, taken through its page and started.
--
-- A tab opens on a page and starts nothing. Pressing the button on it
-- is what asks for an opponent, and that is what most of these tests
-- want to have happened already.
started :: TabId -> State -> State
started tab state = next state (InSetup tab StartPressed)

-- | Where the window lands after an event. Nothing here exits, except
-- where a test says so.
next :: State -> Event -> State
next state event = case update' state event of
  Transition state' _ -> state'
  Exit                -> error "the transition exited"

-- | Did this event end the window?
exits :: State -> Event -> Bool
exits state event = case update' state event of
  Exit         -> True
  Transition{} -> False

-- | A window with one game open on a 9x9 board, whose opponent has
-- started.
oneGame :: State
oneGame = next (started 1 (next empty' NewTabPressed)) (TabOpened 1 (Right silent))

-- | Why the game in a tab stopped, if it did.
brokenBecause :: Session -> Maybe Text
brokenBecause session = case session.opponent of
  Gone why -> Just why
  _        -> Nothing

-- | What this tab's page is asking for, when it is still a page.
chosenIn :: TabId -> State -> Maybe Setup
chosenIn tab state = case lookup tab state.openTabs of
  Just (Choosing setup) -> Just setup
  _                     -> Nothing

-- | The game in this tab, when it has one rather than a page.
gameIn :: TabId -> State -> Maybe Session
gameIn tab state = case lookup tab state.openTabs of
  Just (Playing session) -> Just session
  _                      -> Nothing

prop_theFirstTabOpensOnItsPage :: Property
prop_theFirstTabOpensOnItsPage = withTests 1 . property $ do
  let opening = next empty' NewTabPressed
  map fst empty'.openTabs === []
  map fst opening.openTabs === [1]
  opening.showing === Just 1
  -- A tab opens on the page that asks what to play, which is not a
  -- game and has no opponent behind it.
  chosenIn 1 opening === Just asking
  fmap sessionSize (gameIn 1 opening) === Nothing
  -- Nothing is asked of anybody by opening one.
  null (jobsOf (commandOf (update' empty' NewTabPressed))) === True

prop_startingThePageMakesTheGameItAsksFor :: Property
prop_startingThePageMakesTheGameItAsksFor = withTests 1 . property $ do
  let opening = next empty' NewTabPressed
      chosen  = next opening (InSetup 1 (ChoseBoard 13))
      going   = next chosen (InSetup 1 StartPressed)
  fmap (.size) (chosenIn 1 chosen) === Just 13
  fmap sessionSize (gameIn 1 going) === Just 13
  chosenIn 1 going === Nothing
  -- And it is the one thing on that page that asks for an opponent.
  fmap playable (gameIn 1 going) === Just False
  fmap playable (gameIn 1 oneGame) === Just True

prop_whatWasStartedIsWhatTheNextPageAsksFor :: Property
prop_whatWasStartedIsWhatTheNextPageAsksFor = withTests 1 . property $ do
  -- A second game of the same kind should be one press, so the page
  -- of a new tab opens on whatever was started last.
  let opening = next empty' NewTabPressed
      chosen  = next opening (InSetup 1 (ChoseBoard 13))
      picked  = next chosen (InSetup 1 (ChoseSide White))
      going   = next picked (InSetup 1 StartPressed)
      again   = next going NewTabPressed
  chosenIn 2 again === Just asking { size = 13, human = White }

prop_aChoiceOnOneTabLeavesTheOthersAlone :: Property
prop_aChoiceOnOneTabLeavesTheOthersAlone = withTests 1 . property $ do
  let two    = next (next empty' NewTabPressed) NewTabPressed
      chosen = next two (InSetup 1 (ChoseBoard 13))
  fmap (.size) (chosenIn 1 chosen) === Just 13
  fmap (.size) (chosenIn 2 chosen) === Just asking.size
  -- A choice for a tab that is not there, or for one that is a game
  -- rather than a page, changes nothing.
  map fst (next chosen (InSetup 99 (ChoseBoard 9))).openTabs === [1, 2]
  fmap sessionSize (gameIn 1 (next oneGame (InSetup 1 (ChoseBoard 13))))
    === Just 9

prop_eachTabGetsAGameAndAnOpponentOfItsOwn :: Property
prop_eachTabGetsAGameAndAnOpponentOfItsOwn = withTests 1 . property $ do
  let opening = next oneGame NewTabPressed
      wider   = next opening (InSetup 2 (ChoseBoard 19))
      ready'  = next (started 2 wider) (TabOpened 2 (Right silent))
  map fst ready'.openTabs === [1, 2]
  ready'.showing === Just 2
  fmap sessionSize (gameIn 1 ready') === Just 9
  fmap sessionSize (gameIn 2 ready') === Just 19

prop_aMoveGoesToTheTabItWasPlayedIn :: Property
prop_aMoveGoesToTheTabItWasPlayedIn = withTests 1 . property $ do
  let two    = next (started 2 (next oneGame NewTabPressed)) (TabOpened 2 (Right silent))
      played = next two (InTab 1 (Clicked (Coord 3 3)))
  fmap (\s -> s.game.last) (gameIn 1 played) === Just (Just (Coord 3 3))
  fmap (\s -> s.game.last) (gameIn 2 played) === Just Nothing

prop_anAnswerForAClosedTabIsDropped :: Property
prop_anAnswerForAClosedTabIsDropped = withTests 1 . property $ do
  -- An opponent stopped in the middle of thinking answers with a
  -- failure, and by then its tab is gone.
  let two    = next (started 2 (next oneGame NewTabPressed)) (TabOpened 2 (Right silent))
      closed = next two (TabClosePressed "game-1")
      stray  = next closed (InTab 1 (Answered (Failed "stopped")))
  map fst closed.openTabs === [2]
  map fst stray.openTabs === [2]
  stray.showing === Just 2

prop_closingTheShowingTabShowsAnother :: Property
prop_closingTheShowingTabShowsAnother = withTests 1 . property $ do
  let two    = next (started 2 (next oneGame NewTabPressed)) (TabOpened 2 (Right silent))
      first' = next two (TabSelected "game-1")
      closed = next first' (TabClosePressed "game-1")
  first'.showing === Just 1
  closed.showing === Just 2

prop_closingAnotherTabLeavesTheShowingOneAlone :: Property
prop_closingAnotherTabLeavesTheShowingOneAlone = withTests 1 . property $ do
  let two    = next (started 2 (next oneGame NewTabPressed)) (TabOpened 2 (Right silent))
      closed = next two (TabClosePressed "game-1")
  two.showing === Just 2
  closed.showing === Just 2

prop_theWindowClosesWithItsLastTab :: Property
prop_theWindowClosesWithItsLastTab = withTests 1 . property $ do
  exits oneGame (TabClosePressed "game-1") === True
  -- A tab that is not the last one only closes itself.
  let two = next (started 2 (next oneGame NewTabPressed)) (TabOpened 2 (Right silent))
  exits two (TabClosePressed "game-1") === False

prop_aTabNameNothingAnswersToIsIgnored :: Property
prop_aTabNameNothingAnswersToIsIgnored = withTests 1 . property $ do
  map fst ((next oneGame (TabClosePressed "game-99")).openTabs) === [1]
  (next oneGame (TabSelected "game-99")).showing === Nothing

prop_tabNamesAreNotReusedWhenATabCloses :: Property
prop_tabNamesAreNotReusedWhenATabCloses = withTests 1 . property $ do
  -- A name that came back would send an opponent's answer to whichever
  -- game happened to be holding the name at the time.
  let two    = next (started 2 (next oneGame NewTabPressed)) (TabOpened 2 (Right silent))
      closed = next two (TabClosePressed "game-1")
      third  = next closed NewTabPressed
  map fst third.openTabs === [2, 3]

prop_draggingATabChangesTheOrder :: Property
prop_draggingATabChangesTheOrder = withTests 1 . property $ do
  let opening   = next oneGame NewTabPressed
      wider     = next opening (InSetup 2 (ChoseBoard 19))
      two       = next (started 2 wider) (TabOpened 2 (Right silent))
      reordered = next two (TabsReordered (Vector.fromList ["game-2", "game-1"]))
  map fst two.openTabs === [1, 2]
  map fst reordered.openTabs === [2, 1]
  -- The games themselves move with their tabs.
  fmap sessionSize (gameIn 2 reordered) === Just 19

prop_anOpponentThatWillNotStartBreaksOnlyItsOwnTab :: Property
prop_anOpponentThatWillNotStartBreaksOnlyItsOwnTab = withTests 1 . property $ do
  let two    = started 2 (next oneGame NewTabPressed)
      failed = next two (TabOpened 2 (Left "no such program"))
  fmap brokenBecause (gameIn 2 failed) === Just (Just "no such program")
  fmap playable (gameIn 2 failed) === Just False
  fmap playable (gameIn 1 failed) === Just True

-- * The lines the header bar shows
------------------------------------

-- | A game against the silent opponent, ready for the player to move.
inPlay :: Session
inPlay = landed (opened silent (starting Black 9))

-- | The game a transition leaves behind.
landed :: Played -> Session
landed (Transition session _) = session
landed Exit                   = error "a game ended the window"

-- | Where a game lands after one thing happens to it.
onceMore :: Session -> SessionEvent -> Session
onceMore session event = landed (step session event)

prop_theTitleSaysWhoseTurnItIs :: Property
prop_theTitleSaysWhoseTurnItIs = withTests 1 . property $ do
  stateOf (starting Black 9) === "Starting\8230"
  stateOf inPlay === "Your move"
  stateOf (onceMore inPlay (Clicked (Coord 3 3))) === "Thinking\8230"
  stateOf (couldNotOpen "gone" (starting Black 9)) === "Stopped"

prop_aTabOnItsPageSaysWhatItIsAskingFor :: Property
prop_aTabOnItsPageSaysWhatItIsAskingFor = withTests 1 . property $ do
  let wanted = Setup { size = 13, human = White, strength = Gentle }
  statusOf (Choosing wanted) === "New game"
  detailOf (Choosing wanted) === describeSetup wanted
  detailOf (Choosing wanted) === "13\215\&13  \183  White  \183  Gentle"
  -- And a tab with a game in it says where the game stands.
  statusOf (Playing inPlay) === stateOf inPlay
  detailOf (Playing inPlay) === aboutGame inPlay

prop_theTitleSaysHowTheGameEnded :: Property
prop_theTitleSaysHowTheGameEnded = withTests 1 . property $ do
  let passed   = onceMore inPlay Passed
      bothPass = onceMore passed (Answered (Moved Pass))
      played   = onceMore inPlay (Clicked (Coord 3 3))
      theirs   = onceMore played (Answered (Moved Resign))
      mine     = onceMore inPlay ResignPressed
  stateOf bothPass === "Game over"
  stateOf theirs === "The engine resigned"
  stateOf mine === "You resigned"

prop_theSubtitleCarriesTheNumbers :: Property
prop_theSubtitleCarriesTheNumbers = withTests 1 . property $ do
  aboutGame inPlay === capturesOf inPlay
  capturesOf inPlay === "9\215\&9  \183  Black 0  \183  White 0"
  capturesOf (starting White 19) === "19\215\&19  \183  Black 0  \183  White 0"

prop_theSubtitleGivesWayToNews :: Property
prop_theSubtitleGivesWayToNews = withTests 1 . property $ do
  -- A score at the end, a point the rules would not take, and an
  -- engine that has gone all have more to say than the counts do.
  let passed = onceMore inPlay Passed
      scored = onceMore passed (Answered (Scored "B+2.5"))
  aboutGame scored === "Result: B+2.5"

  let played = onceMore inPlay (Clicked (Coord 3 3))
      back   = onceMore played (Answered (Moved (Play (Coord 4 4))))
      onTop  = onceMore back (Clicked (Coord 3 3))
  aboutGame onTop === describeIllegal Occupied

  aboutGame (couldNotOpen "no such program" (starting Black 9))
    === "no such program"

prop_aWindowWithNoGameShowingSaysSo :: Property
prop_aWindowWithNoGameShowingSaysSo = withTests 1 . property $ do
  -- The window before its first tab has opened, and after a tab has
  -- been selected by a name nothing answers to.
  empty'.showing === Nothing
  map fst empty'.openTabs === []

prop_closingTheWindowEndsIt :: Property
prop_closingTheWindowEndsIt = withTests 1 . property $ do
  exits oneGame Closed === True
  exits empty' Closed === True

prop_aTabWhoseOpponentIsStillStartingCanBeClosed :: Property
prop_aTabWhoseOpponentIsStillStartingCanBeClosed =
  withTests 1 . property $ do
    -- There is no opponent to stop yet, so closing the tab only takes
    -- the tab away.
    let opening = next oneGame NewTabPressed
        closed  = next opening (TabClosePressed "game-2")
    map fst opening.openTabs === [1, 2]
    map fst closed.openTabs === [1]
    closed.showing === Just 1

prop_aTabWhoseOpponentIsGoneCanBeClosed :: Property
prop_aTabWhoseOpponentIsGoneCanBeClosed = withTests 1 . property $ do
  let broken = next (started 2 (next oneGame NewTabPressed))
                    (TabOpened 2 (Left "no such program"))
      closed = next broken (TabClosePressed "game-2")
  map fst closed.openTabs === [1]

prop_closingATabWhileItsOpponentThinksStopsItToo :: Property
prop_closingATabWhileItsOpponentThinksStopsItToo =
  withTests 1 . property $ do
    let two      = next (next oneGame NewTabPressed)
                        (TabOpened 2 (Right silent))
        thinking = next two (InTab 1 (Clicked (Coord 3 3)))
        closed   = next thinking (TabClosePressed "game-1")
    map fst closed.openTabs === [2]

prop_closingTheLastTabInTheListShowsTheOneBeforeIt :: Property
prop_closingTheLastTabInTheListShowsTheOneBeforeIt =
  withTests 1 . property $ do
    let two    = next (next oneGame NewTabPressed)
                      (TabOpened 2 (Right silent))
        closed = next two (TabClosePressed "game-2")
    two.showing === Just 2
    map fst closed.openTabs === [1]
    closed.showing === Just 1

prop_reorderingByNamesNothingAnswersToKeepsWhatItKnows :: Property
prop_reorderingByNamesNothingAnswersToKeepsWhatItKnows =
  withTests 1 . property $ do
    let two = next (next oneGame NewTabPressed)
                   (TabOpened 2 (Right silent))
        odd' = next two (TabsReordered (Vector.fromList ["game-2", "game-99"]))
    -- A name nothing answers to brings no game with it, so it drops
    -- out rather than becoming a tab with nothing in it.
    map fst odd'.openTabs === [2]

-- * What the window asks somebody to do
-----------------------------------------

prop_startingAGameStartsAnOpponentForIt :: Property
prop_startingAGameStartsAnOpponentForIt = withTests 1 . property $ do
  answer <- ran (next empty' NewTabPressed) (InSetup 1 StartPressed)
  case answer of
    [TabOpened 1 (Right engine)] -> engine.name === "nobody"
    _ -> annotate "it did not start one" >> failure

prop_aMoveIsPutToTheOpponentOfItsOwnTab :: Property
prop_aMoveIsPutToTheOpponentOfItsOwnTab = withTests 1 . property $ do
  answer <- ran oneGame (InTab 1 (Clicked (Coord 3 3)))
  -- The silent opponent passes, and the answer comes back addressed to
  -- the tab the move was played in.
  case answer of
    [InTab 1 (Answered (Moved Pass))] -> success
    _ -> annotate "it came back to the wrong tab" >> failure

prop_closingATabStopsTheOpponentThatWasPlayingInIt :: Property
prop_closingATabStopsTheOpponentThatWasPlayingInIt =
  withTests 1 . property $ do
    counts <- evalIO $ do
      (opponents, counts) <- countingSource
      one     <- acting (startingState opponents asking) NewTabPressed
      going   <- acting one (InSetup 1 StartPressed)
      ready'  <- acting going (TabOpened 1 (Right silent))
      two     <- acting ready' NewTabPressed
      going'  <- acting two (InSetup 2 StartPressed)
      both    <- acting going' (TabOpened 2 (Right silent))
      _       <- acting both (TabClosePressed "game-1")
      readIORef counts
    -- Two were started, and the one whose tab closed was given back.
    counts === (2, 1)

prop_closingATabWhileItIsThinkingStopsItAsWell :: Property
prop_closingATabWhileItIsThinkingStopsItAsWell =
  withTests 1 . property $ do
    counts <- evalIO $ do
      (opponents, counts) <- countingSource
      one      <- acting (startingState opponents asking) NewTabPressed
      going    <- acting one (InSetup 1 StartPressed)
      ready'   <- acting going (TabOpened 1 (Right silent))
      two      <- acting ready' NewTabPressed
      going'   <- acting two (InSetup 2 StartPressed)
      both     <- acting going' (TabOpened 2 (Right silent))
      thinking <- acting both (InTab 1 (Clicked (Coord 3 3)))
      _        <- acting thinking (TabClosePressed "game-1")
      readIORef counts
    counts === (2, 1)

prop_closingTabsWithNoOpponentStopsNothing :: Property
prop_closingTabsWithNoOpponentStopsNothing = withTests 1 . property $ do
  counts <- evalIO $ do
    (opponents, counts) <- countingSource
    one     <- acting (startingState opponents asking) NewTabPressed
    going   <- acting one (InSetup 1 StartPressed)
    ready'  <- acting going (TabOpened 1 (Right silent))
    -- One tab whose opponent never started, and one still on its page.
    broken  <- acting ready' NewTabPressed
    asked'  <- acting broken (InSetup 2 StartPressed)
    failed' <- acting asked' (TabOpened 2 (Left "no such program"))
    opening <- acting failed' NewTabPressed
    closed  <- acting opening (TabClosePressed "game-2")
    _       <- acting closed (TabClosePressed "game-3")
    readIORef counts
  -- Two games were started and one opponent came back. Neither of the
  -- tabs that closed had one to give back: the first never got one,
  -- and the second was still on its page.
  counts === (2, 0)

prop_closingATabWithNoOpponentYetStopsNothing :: Property
prop_closingATabWithNoOpponentYetStopsNothing = withTests 1 . property $ do
  let opening = next oneGame NewTabPressed
  null (jobsOf (commandOf (update' opening (TabClosePressed "game-2"))))
    === True

prop_theWindowSendsItselfItsFirstGame :: Property
prop_theWindowSendsItselfItsFirstGame = withTests 1 . property $ do
  -- The window opens with nothing in it and one event on its way,
  -- which is what puts the first tab up. That tab opens on a page, so
  -- the event asks for a tab and not for a board.
  let wider                      = asking { size = 13 } :: Setup
      App { inputs = producers } = application source wider
  events <- evalIO (concat <$> traverse Pipes.toListM producers)
  case events of
    [NewTabPressed] -> success
    _               -> annotate "it asked for something else" >> failure

prop_theWindowIsWiredToItsOwnUpdateAndView :: Property
prop_theWindowIsWiredToItsOwnUpdateAndView = withTests 1 . property $ do
  -- Written out rather than updated, because `human` is a field of a
  -- game as well and a record update cannot be told which is meant.
  let playingWhite =
        Setup { size = asking.size, human = White, strength = asking.strength }
      built = application source playingWhite
  (initialState built).opening.human === White
  case update built (initialState built) NewTabPressed of
    Transition state' _ -> map fst state'.openTabs === [1]
    Exit                -> annotate "it exited" >> failure

-- | Take an event, do whatever it asked for, and answer with where the
-- window landed. This is what the application does, without the
-- window.
acting :: State -> Event -> IO State
acting state event = case update' state event of
  Exit                -> pure state
  Transition state' cmd ->
    state' <$ traverse_ (Pipes.toListM . snd) (jobsOf cmd)

tests :: Group
tests = $$(discover)