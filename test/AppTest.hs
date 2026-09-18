{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The tabs: which games are open, which one is showing, and where
-- each answer belongs.
module AppTest
  ( tests
  )
where

import           Data.IORef
import           Data.Text                      ( Text )
import           Hedgehog
import qualified Pipes.Prelude                 as Pipes
import qualified Data.Vector                   as Vector

import           GI.Gtk.Declarative.App.Simple  ( App(..)
                                                , Transition(..)
                                                )

import           Go.Game
import           Go.Types
import           Stones.App
import           Stones.Engine
import           Stones.Session

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

-- | A source of opponents that hands out the same silent one.
source :: Opponents
source = Opponents { openOpponent  = \_ -> pure (Right silent)
                   , closeOpponent = \_ -> pure ()
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
    ( Opponents { openOpponent  = \_ -> Right silent <$ handedOut
                , closeOpponent = \_ -> handedBack
                }
    , counts
    )

-- | Run what an event asked somebody to do, and answer with the event
-- it came back with.
ran :: State -> Event -> PropertyT IO (Maybe Event)
ran state event = case decide state event of
  Close                 -> annotate "the window closed" >> failure
  Carry _ Nothing       -> annotate "nothing was asked" >> failure
  Carry _ (Just job)    -> evalIO job

-- | A window with nothing open in it, playing Black.
empty' :: State
empty' = startingState source Black

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
oneGame = next (next empty' (NewTabPressed 9)) (TabOpened 1 (Right silent))

-- | Why the game in a tab stopped, if it did.
brokenBecause :: Session -> Maybe Text
brokenBecause session = case sessionOpponent session of
  Gone why -> Just why
  _        -> Nothing

-- | The game in this tab.
gameIn :: TabId -> State -> Maybe Session
gameIn tab state = lookup tab (stateGames state)

prop_theFirstTabIsOpenedAndShown :: Property
prop_theFirstTabIsOpenedAndShown = withTests 1 . property $ do
  let opening = next empty' (NewTabPressed 9)
  map fst (stateGames empty') === []
  map fst (stateGames opening) === [1]
  stateShowing opening === Just 1
  -- It has no opponent until one has started, so it takes no moves.
  fmap playable (gameIn 1 opening) === Just False
  fmap playable (gameIn 1 oneGame) === Just True

prop_eachTabGetsAGameAndAnOpponentOfItsOwn :: Property
prop_eachTabGetsAGameAndAnOpponentOfItsOwn = withTests 1 . property $ do
  let two   = next oneGame (NewTabPressed 19)
      ready' = next two (TabOpened 2 (Right silent))
  map fst (stateGames ready') === [1, 2]
  stateShowing ready' === Just 2
  fmap sessionSize (gameIn 1 ready') === Just 9
  fmap sessionSize (gameIn 2 ready') === Just 19

prop_aMoveGoesToTheTabItWasPlayedIn :: Property
prop_aMoveGoesToTheTabItWasPlayedIn = withTests 1 . property $ do
  let two    = next (next oneGame (NewTabPressed 19)) (TabOpened 2 (Right silent))
      played = next two (InTab 1 (Clicked (Coord 3 3)))
  fmap (gameLast . sessionGame) (gameIn 1 played) === Just (Just (Coord 3 3))
  fmap (gameLast . sessionGame) (gameIn 2 played) === Just Nothing

prop_anAnswerForAClosedTabIsDropped :: Property
prop_anAnswerForAClosedTabIsDropped = withTests 1 . property $ do
  -- An opponent stopped in the middle of thinking answers with a
  -- failure, and by then its tab is gone.
  let two    = next (next oneGame (NewTabPressed 19)) (TabOpened 2 (Right silent))
      closed = next two (TabClosePressed "game-1")
      stray  = next closed (InTab 1 (Answered (Failed "stopped")))
  map fst (stateGames closed) === [2]
  map fst (stateGames stray) === [2]
  stateShowing stray === Just 2

prop_closingTheShowingTabShowsAnother :: Property
prop_closingTheShowingTabShowsAnother = withTests 1 . property $ do
  let two    = next (next oneGame (NewTabPressed 19)) (TabOpened 2 (Right silent))
      first' = next two (TabSelected "game-1")
      closed = next first' (TabClosePressed "game-1")
  stateShowing first' === Just 1
  stateShowing closed === Just 2

prop_closingAnotherTabLeavesTheShowingOneAlone :: Property
prop_closingAnotherTabLeavesTheShowingOneAlone = withTests 1 . property $ do
  let two    = next (next oneGame (NewTabPressed 19)) (TabOpened 2 (Right silent))
      closed = next two (TabClosePressed "game-1")
  stateShowing two === Just 2
  stateShowing closed === Just 2

prop_theWindowClosesWithItsLastTab :: Property
prop_theWindowClosesWithItsLastTab = withTests 1 . property $ do
  exits oneGame (TabClosePressed "game-1") === True
  -- A tab that is not the last one only closes itself.
  let two = next (next oneGame (NewTabPressed 19)) (TabOpened 2 (Right silent))
  exits two (TabClosePressed "game-1") === False

prop_aTabNameNothingAnswersToIsIgnored :: Property
prop_aTabNameNothingAnswersToIsIgnored = withTests 1 . property $ do
  map fst (stateGames (next oneGame (TabClosePressed "game-99"))) === [1]
  stateShowing (next oneGame (TabSelected "game-99")) === Nothing

prop_tabNamesAreNotReusedWhenATabCloses :: Property
prop_tabNamesAreNotReusedWhenATabCloses = withTests 1 . property $ do
  -- A name that came back would send an opponent's answer to whichever
  -- game happened to be holding the name at the time.
  let two    = next (next oneGame (NewTabPressed 19)) (TabOpened 2 (Right silent))
      closed = next two (TabClosePressed "game-1")
      third  = next closed (NewTabPressed 9)
  map fst (stateGames third) === [2, 3]

prop_draggingATabChangesTheOrder :: Property
prop_draggingATabChangesTheOrder = withTests 1 . property $ do
  let two      = next (next oneGame (NewTabPressed 19)) (TabOpened 2 (Right silent))
      reordered = next two (TabsReordered (Vector.fromList ["game-2", "game-1"]))
  map fst (stateGames two) === [1, 2]
  map fst (stateGames reordered) === [2, 1]
  -- The games themselves move with their tabs.
  fmap sessionSize (gameIn 2 reordered) === Just 19

prop_anOpponentThatWillNotStartBreaksOnlyItsOwnTab :: Property
prop_anOpponentThatWillNotStartBreaksOnlyItsOwnTab = withTests 1 . property $ do
  let two    = next oneGame (NewTabPressed 19)
      failed = next two (TabOpened 2 (Left "no such program"))
  fmap brokenBecause (gameIn 2 failed) === Just (Just "no such program")
  fmap playable (gameIn 2 failed) === Just False
  fmap playable (gameIn 1 failed) === Just True

-- * The lines the header bar shows
------------------------------------

-- | A game against the silent opponent, ready for the player to move.
started :: Session
started = stepSession (opened silent (starting Black 9))

-- | Where a game lands after one thing happens to it.
onceMore :: Session -> SessionEvent -> Session
onceMore session event = stepSession (step session event)

prop_theTitleSaysWhoseTurnItIs :: Property
prop_theTitleSaysWhoseTurnItIs = withTests 1 . property $ do
  statusOf (starting Black 9) === "Starting\8230"
  statusOf started === "Your move"
  statusOf (onceMore started (Clicked (Coord 3 3))) === "Thinking\8230"
  statusOf (couldNotOpen "gone" (starting Black 9)) === "Stopped"

prop_theTitleSaysHowTheGameEnded :: Property
prop_theTitleSaysHowTheGameEnded = withTests 1 . property $ do
  let passed   = onceMore started Passed
      bothPass = onceMore passed (Answered (Moved Pass))
      played   = onceMore started (Clicked (Coord 3 3))
      theirs   = onceMore played (Answered (Moved Resign))
      mine     = onceMore started ResignPressed
  statusOf bothPass === "Game over"
  statusOf theirs === "The engine resigned"
  statusOf mine === "You resigned"

prop_theSubtitleCarriesTheNumbers :: Property
prop_theSubtitleCarriesTheNumbers = withTests 1 . property $ do
  detailOf started === capturesOf started
  capturesOf started === "9\215\&9  \183  Black 0  \183  White 0"
  capturesOf (starting White 19) === "19\215\&19  \183  Black 0  \183  White 0"

prop_theSubtitleGivesWayToNews :: Property
prop_theSubtitleGivesWayToNews = withTests 1 . property $ do
  -- A score at the end, a point the rules would not take, and an
  -- engine that has gone all have more to say than the counts do.
  let passed = onceMore started Passed
      scored = onceMore passed (Answered (Scored "B+2.5"))
  detailOf scored === "Result: B+2.5"

  let played = onceMore started (Clicked (Coord 3 3))
      back   = onceMore played (Answered (Moved (Play (Coord 4 4))))
      onTop  = onceMore back (Clicked (Coord 3 3))
  detailOf onTop === describeIllegal Occupied

  detailOf (couldNotOpen "no such program" (starting Black 9))
    === "no such program"

prop_aWindowWithNoGameShowingSaysSo :: Property
prop_aWindowWithNoGameShowingSaysSo = withTests 1 . property $ do
  -- The window before its first tab has opened, and after a tab has
  -- been selected by a name nothing answers to.
  stateShowing empty' === Nothing
  map fst (stateGames empty') === []

prop_closingTheWindowEndsIt :: Property
prop_closingTheWindowEndsIt = withTests 1 . property $ do
  exits oneGame Closed === True
  exits empty' Closed === True

prop_aTabWhoseOpponentIsStillStartingCanBeClosed :: Property
prop_aTabWhoseOpponentIsStillStartingCanBeClosed =
  withTests 1 . property $ do
    -- There is no opponent to stop yet, so closing the tab only takes
    -- the tab away.
    let opening = next oneGame (NewTabPressed 19)
        closed  = next opening (TabClosePressed "game-2")
    map fst (stateGames opening) === [1, 2]
    map fst (stateGames closed) === [1]
    stateShowing closed === Just 1

prop_aTabWhoseOpponentIsGoneCanBeClosed :: Property
prop_aTabWhoseOpponentIsGoneCanBeClosed = withTests 1 . property $ do
  let broken = next (next oneGame (NewTabPressed 19))
                    (TabOpened 2 (Left "no such program"))
      closed = next broken (TabClosePressed "game-2")
  map fst (stateGames closed) === [1]

prop_closingATabWhileItsOpponentThinksStopsItToo :: Property
prop_closingATabWhileItsOpponentThinksStopsItToo =
  withTests 1 . property $ do
    let two      = next (next oneGame (NewTabPressed 19))
                        (TabOpened 2 (Right silent))
        thinking = next two (InTab 1 (Clicked (Coord 3 3)))
        closed   = next thinking (TabClosePressed "game-1")
    map fst (stateGames closed) === [2]

prop_closingTheLastTabInTheListShowsTheOneBeforeIt :: Property
prop_closingTheLastTabInTheListShowsTheOneBeforeIt =
  withTests 1 . property $ do
    let two    = next (next oneGame (NewTabPressed 19))
                      (TabOpened 2 (Right silent))
        closed = next two (TabClosePressed "game-2")
    stateShowing two === Just 2
    map fst (stateGames closed) === [1]
    stateShowing closed === Just 1

prop_reorderingByNamesNothingAnswersToKeepsWhatItKnows :: Property
prop_reorderingByNamesNothingAnswersToKeepsWhatItKnows =
  withTests 1 . property $ do
    let two = next (next oneGame (NewTabPressed 19))
                   (TabOpened 2 (Right silent))
        odd' = next two (TabsReordered (Vector.fromList ["game-2", "game-99"]))
    -- A name nothing answers to brings no game with it, so it drops
    -- out rather than becoming a tab with nothing in it.
    map fst (stateGames odd') === [2]

-- * What the window asks somebody to do
-----------------------------------------

prop_openingATabStartsAnOpponentForIt :: Property
prop_openingATabStartsAnOpponentForIt = withTests 1 . property $ do
  answer <- ran empty' (NewTabPressed 9)
  case answer of
    Just (TabOpened 1 (Right engine)) -> engineName engine === "nobody"
    _ -> annotate "it did not start one" >> failure

prop_aMoveIsPutToTheOpponentOfItsOwnTab :: Property
prop_aMoveIsPutToTheOpponentOfItsOwnTab = withTests 1 . property $ do
  answer <- ran oneGame (InTab 1 (Clicked (Coord 3 3)))
  -- The silent opponent passes, and the answer comes back addressed to
  -- the tab the move was played in.
  case answer of
    Just (InTab 1 (Answered (Moved Pass))) -> success
    _ -> annotate "it came back to the wrong tab" >> failure

prop_closingATabStopsTheOpponentThatWasPlayingInIt :: Property
prop_closingATabStopsTheOpponentThatWasPlayingInIt =
  withTests 1 . property $ do
    counts <- evalIO $ do
      (opponents, counts) <- countingSource
      one    <- acting (startingState opponents Black) (NewTabPressed 9)
      ready' <- acting one (TabOpened 1 (Right silent))
      two    <- acting ready' (NewTabPressed 19)
      both   <- acting two (TabOpened 2 (Right silent))
      _      <- acting both (TabClosePressed "game-1")
      readIORef counts
    -- Two were started, and the one whose tab closed was given back.
    counts === (2, 1)

prop_closingATabWhileItIsThinkingStopsItAsWell :: Property
prop_closingATabWhileItIsThinkingStopsItAsWell =
  withTests 1 . property $ do
    counts <- evalIO $ do
      (opponents, counts) <- countingSource
      one      <- acting (startingState opponents Black) (NewTabPressed 9)
      ready'   <- acting one (TabOpened 1 (Right silent))
      two      <- acting ready' (NewTabPressed 19)
      both     <- acting two (TabOpened 2 (Right silent))
      thinking <- acting both (InTab 1 (Clicked (Coord 3 3)))
      _        <- acting thinking (TabClosePressed "game-1")
      readIORef counts
    counts === (2, 1)

prop_closingTabsWithNoOpponentStopsNothing :: Property
prop_closingTabsWithNoOpponentStopsNothing = withTests 1 . property $ do
  counts <- evalIO $ do
    (opponents, counts) <- countingSource
    one     <- acting (startingState opponents Black) (NewTabPressed 9)
    ready'  <- acting one (TabOpened 1 (Right silent))
    -- One tab whose opponent never started, and one still starting.
    broken  <- acting ready' (NewTabPressed 13)
    failed' <- acting broken (TabOpened 2 (Left "no such program"))
    opening <- acting failed' (NewTabPressed 19)
    closed  <- acting opening (TabClosePressed "game-2")
    _       <- acting closed (TabClosePressed "game-3")
    readIORef counts
  -- Three tabs were opened, one opponent came back, and neither of the
  -- two tabs that closed had one to give back.
  counts === (3, 0)

prop_closingATabWithNoOpponentYetStopsNothing :: Property
prop_closingATabWithNoOpponentYetStopsNothing = withTests 1 . property $ do
  let opening = next oneGame (NewTabPressed 19)
  case decide opening (TabClosePressed "game-2") of
    Carry _ Nothing -> success
    _               -> annotate "it asked for something" >> failure

prop_theWindowSendsItselfItsFirstGame :: Property
prop_theWindowSendsItselfItsFirstGame = withTests 1 . property $ do
  -- The window opens with nothing in it and one event on its way,
  -- which is what puts the first board up.
  let App { inputs = producers } = application source Black 13
  events <- evalIO (concat <$> traverse Pipes.toListM producers)
  case events of
    [NewTabPressed 13] -> success
    _                  -> annotate "it asked for something else" >> failure

prop_theWindowIsWiredToItsOwnUpdateAndView :: Property
prop_theWindowIsWiredToItsOwnUpdateAndView = withTests 1 . property $ do
  let built = application source White 9
  stateHuman (initialState built) === White
  case update built (initialState built) (NewTabPressed 9) of
    Transition state' _ -> map fst (stateGames state') === [1]
    Exit                -> annotate "it exited" >> failure

-- | Take an event, do whatever it asked for, and answer with where the
-- window landed. This is what the application does, without the
-- window.
acting :: State -> Event -> IO State
acting state event = case decide state event of
  Close            -> pure state
  Carry state' job -> state' <$ sequence_ job

tests :: Group
tests = $$(discover)