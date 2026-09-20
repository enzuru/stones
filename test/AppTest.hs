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
import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text )
import           Hedgehog
import qualified Pipes.Prelude                 as Pipes
import qualified Data.Vector                   as Vector

import qualified Data.List                     as List
import qualified Data.Text                     as Text
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           GI.Gtk.Declarative.App.Simple  ( App(..)
                                                , Cmd
                                                , Transition(..)
                                                , jobsOf
                                                , none
                                                , subKey
                                                )

import           Go.Game
import           Go.Types
import           Stones.App
import           Stones.Opponent
import           Stones.Session
import           Stones.Menu                    ( Choice(..)
                                                , MenuProps(..)
                                                , choices
                                                )
import           Stones.Setup

-- | An opponent that does nothing and says so.
silent :: Opponent
silent = Opponent { name  = "nobody"
                  , tell  = \_ -> pure (Right ())
                  , hear  = pure ()
                  , leave = pure ()
                  , talks = True
                  , again = Just "nobody"
                  }

-- | One that is a program on this machine, which can be stopped and
-- started again whenever anybody likes.
program :: Opponent
program = silent { name = "GNU Go", talks = False, again = Nothing }

-- | A source of opponents that hands out the same silent one.
source :: Opponents
source = Opponents { open  = \_ -> pure (Right silent)
                   , close = \_ -> pure ()
                   , enter = pure (Right "nobody")
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
    ( Opponents { open  = \_ -> Right silent <$ handedOut
                , close = \_ -> handedBack
                , enter = pure (Right "nobody")
                }
    , counts
    )

-- | Run what an event asked somebody to do, and answer with the events
-- it came back with.
ran :: State -> Event -> PropertyT IO [Event]
ran state event = case jobsOf (commandOf (update' state event)) of
  []   -> annotate "nothing was asked" >> failure
  jobs -> concat <$> traverse (evalIO . Pipes.toListM . snd) jobs

-- | The names the jobs of an event run under.
namesOf :: State -> Event -> [Maybe Text]
namesOf state event = map fst (jobsOf (commandOf (update' state event)))

-- | The command a transition answers with.
commandOf :: Transition state event -> Cmd event
commandOf (Transition _ cmd) = cmd
commandOf Exit               = none


-- | A game set up on a board this wide, as this colour, against a
-- program on this machine.
board :: Color -> Int -> Setup
board side n = defaultSetup { against = Computer, size = n, human = side }

-- | A window with nothing open in it, whose page asks for a 9x9 game
-- as Black.
empty' :: State
empty' = startingState source asking

-- | What the page of a new tab asks for, in these tests.
asking :: Setup
asking = Setup { against = Computer, size = 9, human = Black, strength = Fierce, handicap = 0, speed = Live, ranked = False, handicapping = Standard, versus = "" }

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
brokenBecause session = case session.standing of
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
      stray  = next closed (InTab 1 (Arrived (Lost "stopped")))
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
inPlay = landed (opened silent (starting (board Black 9)))

-- | The game a transition leaves behind.
landed :: Step -> Session
landed (Transition session _) = session
landed Exit                   = error "a game ended the window"

-- | Where a game lands after one thing happens to it.
onceMore :: Session -> SessionEvent -> Session
onceMore session event = landed (step session event)

prop_theTitleSaysWhoseTurnItIs :: Property
prop_theTitleSaysWhoseTurnItIs = withTests 1 . property $ do
  stateOf (starting (board Black 9)) === "Starting\8230"
  stateOf inPlay === "Your move"
  stateOf (onceMore inPlay (Clicked (Coord 3 3))) === "Thinking\8230"
  stateOf (couldNotOpen "gone" (starting (board Black 9))) === "Stopped"

prop_aTabOnItsPageSaysWhatItIsAskingFor :: Property
prop_aTabOnItsPageSaysWhatItIsAskingFor = withTests 1 . property $ do
  let wanted =
        Setup { against = Computer, size = 13, human = White, strength = Gentle, handicap = 0, speed = Live, ranked = False, handicapping = Standard, versus = "" }
  statusOf (Choosing wanted) === "New game"
  detailOf (Choosing wanted) === describeSetup wanted
  detailOf (Choosing wanted) === "13\215\&13  \183  White  \183  Gentle"
  -- And a tab with a game in it says where the game stands.
  statusOf (Playing inPlay) === stateOf inPlay
  detailOf (Playing inPlay) === aboutGame inPlay

prop_theTitleSaysHowTheGameEnded :: Property
prop_theTitleSaysHowTheGameEnded = withTests 1 . property $ do
  let passed   = onceMore inPlay Passed
      bothPass = onceMore passed (Arrived (Played White Pass))
      played   = onceMore inPlay (Clicked (Coord 3 3))
      theirs   = onceMore played (Arrived (Played White Resign))
      mine     = onceMore inPlay ResignPressed
  stateOf bothPass === "Game over"
  -- Whoever gave up is named, because the other side is a program in
  -- one game and a person in the next, and "the engine" is wrong in
  -- the second.
  stateOf theirs === opponentOf theirs <> " resigned"
  stateOf theirs === "nobody resigned"
  stateOf mine === "You resigned"

-- * Starting over
-------------------

prop_startingOverAsksForWhatTheGameWasStartedFrom :: Property
prop_startingOverAsksForWhatTheGameWasStartedFrom =
  withTests 1 . property $ do
    -- What the game was started from, not what the page is asking for
    -- now. Somebody who changed the page and then pressed this on an
    -- old game meant the old game.
    asked <- evalIO (newIORef [])
    let noting = source
          { open = \setup -> Right program <$ modifyIORef' asked (<> [setup])
          }
        started' = next (started 1 (next (startingState noting asking) NewTabPressed))
                        (TabOpened 1 (Right program))
        -- The page of a second tab is changed to something else, which
        -- is what the window would start next.
        moved = next (next started' NewTabPressed) (InSetup 2 (ChoseBoard 19))
    _    <- ran moved (AgainPressed 1)
    went <- evalIO (readIORef asked)
    went === [asking]

prop_playingAgainOnAServerAsksTheSamePlayer :: Property
prop_playingAgainOnAServerAsksTheSamePlayer = property $ do
  -- Somebody paired by the server, whose name nobody here knew until
  -- the game started.
  asked <- evalIO (newIORef [])
  let stranger = silent { name = "hikaru", again = Just "hikaru" }
      noting   = source
        { open = \setup -> Right stranger <$ modifyIORef' asked (<> [setup])
        }
      played = next
        (started 1 (next (startingState noting asking) NewTabPressed))
        (TabOpened 1 (Right stranger))
      -- The game has to have ended, because walking out of one that
      -- has not is leaving rather than starting over.
      over   = next played (InTab 1 ResignPressed)
  _    <- ran over (AgainPressed 1)
  went <- evalIO (readIORef asked)
  went === [asking { versus = "hikaru" }]

prop_startingOverLetsTheOldOpponentGo :: Property
prop_startingOverLetsTheOldOpponentGo = property $ do
  -- On a server an opponent left running is a board somebody else is
  -- still sitting at.
  (counting', counts) <- evalIO countingSource
  let playing = next (started 1 (next (startingState counting' asking) NewTabPressed))
                     (TabOpened 1 (Right silent { talks = False }))
  _           <- ran playing (AgainPressed 1)
  (_, back) <- evalIO (readIORef counts)
  back === 1

prop_startingOverPutsAFreshGameInTheSameTab :: Property
prop_startingOverPutsAFreshGameInTheSameTab = withTests 1 . property $ do
  let against = next (started 1 (next empty' NewTabPressed))
                     (TabOpened 1 (Right program))
      played  = next against (InTab 1 (Clicked (Coord 3 3)))
      afresh  = next played (AgainPressed 1)
  -- The board is empty again and the tab is the same tab.
  map fst afresh.openTabs === [1]
  fmap (capturedBy Black . (.captures) . (.game)) (gameIn 1 afresh) === Just 0
  fmap (stateOf) (gameIn 1 afresh) === Just "Starting\8230"
  -- And it runs under the tab's own name, so closing the tab stops it.
  namesOf played (AgainPressed 1) === [Nothing, Just "finding game-1"]

prop_startingOverDoesNothingToAGameSomebodyIsWaitingOn :: Property
prop_startingOverDoesNothingToAGameSomebodyIsWaitingOn =
  withTests 1 . property $ do
    -- The button is greyed, so this only arrives if something else
    -- sends it, and the answer is the same either way.
    canPlayAgain inPlay === False
    null (jobsOf (commandOf (update' oneGame (AgainPressed 1)))) === True
    -- And a tab that is still a page has no game to start over.
    null (jobsOf (commandOf (update' (next empty' NewTabPressed) (AgainPressed 1))))
      === True

-- * Talking
-------------

-- | A window with two games, showing the first.
bothOpen :: State
bothOpen = twoGames { showing = Just 1 }

prop_somethingSaidInATabBehindThisOneIsShownOverIt :: Property
prop_somethingSaidInATabBehindThisOneIsShownOverIt =
  withTests 1 . property $ do
    -- A message that arrives in a tab nobody is looking at is a
    -- message nobody sees.
    let said = next bothOpen (InTab 2 (Arrived (Spoke "hikaru" "good game")))
    map (.line) said.asides === ["hikaru: good game"]
    -- And it is in the game it was said about as well, so opening that
    -- tab shows it where the rest of the talking is.
    map (.what) (maybe [] (.said) (gameIn 2 said)) === ["good game"]

prop_somethingSaidInTheTabInFrontWithThePaneShutIsShownToo :: Property
prop_somethingSaidInTheTabInFrontWithThePaneShutIsShownToo =
  withTests 1 . property $ do
    -- The pane is what they would have read it in, and it is shut.
    let shut = next bothOpen (InTab 1 (Arrived (Spoke "hikaru" "hello")))
    map (.line) shut.asides === ["hikaru: hello"]
    -- With the pane open they are looking at it, so nothing is put
    -- over the board.
    let open' = next (next bothOpen (ChatToggled True))
                     (InTab 1 (Arrived (Spoke "hikaru" "hello")))
    open'.asides === []

prop_somethingShownOverTheBoardIsForgottenWhenItGoes :: Property
prop_somethingShownOverTheBoardIsForgottenWhenItGoes =
  withTests 1 . property $ do
    let said = next bothOpen (InTab 2 (Arrived (Spoke "hikaru" "good game")))
    case said.asides of
      [aside] -> do
        (next said (AsideGone aside.key)).asides === []
        -- One that is not there is not an error, because a window can
        -- be told twice.
        (next said (AsideGone "no such thing")).asides === said.asides
      _ -> annotate "it kept the wrong number" >> failure

prop_theSameWordsTwiceAreTwoThingsToShow :: Property
prop_theSameWordsTwiceAreTwoThingsToShow = withTests 1 . property $ do
  -- The overlay shows a name it has not shown and leaves one it has
  -- alone, so two of these under one name would be shown once.
  let twice = foldl (\state _ -> next state (InTab 2 (Arrived (Spoke "hikaru" "hello"))))
                    bothOpen
                    [1 .. 2 :: Int]
  length twice.asides === 2
  length (List.nub (map (.key) twice.asides)) === 2

prop_noMoreAreKeptThanAnybodyReads :: Property
prop_noMoreAreKeptThanAnybodyReads = property $ do
  many <- forAll (Gen.int (Range.linear 0 20))
  let said = foldl
        (\state n ->
          next state (InTab 2 (Arrived (Spoke "hikaru" (Text.pack (show n)))))
        )
        bothOpen
        [1 .. many]
  -- The list only shrinks when the overlay says one has gone, so it is
  -- capped rather than trusted.
  assert (length said.asides <= asidesKept)
  -- And what is kept is the newest, because that is what is worth
  -- reading.
  assert (length said.asides == min many asidesKept)

-- * Looking for an opponent
-----------------------------

prop_theSearchForAnOpponentRunsUnderItsTabsName :: Property
prop_theSearchForAnOpponentRunsUnderItsTabsName =
  withTests 1 . property $ do
    -- A name is what the loop stops a job by, and finding an opponent
    -- on a server is waiting for somebody else to want the same game,
    -- which can take minutes.
    let opening = next empty' NewTabPressed
    namesOf opening (InSetup 1 StartPressed) === [Just "finding game-1"]
    -- One name for each tab, so stopping one leaves the other looking.
    let two = next (started 1 opening) NewTabPressed
    namesOf two (InSetup 2 StartPressed) === [Just "finding game-2"]

prop_closingATabWhileItIsStillLookingStopsTheLooking :: Property
prop_closingATabWhileItIsStillLookingStopsTheLooking =
  withTests 1 . property $ do
    -- Under the same name as the search, which is how the loop is told
    -- to stop it. A search left running pairs somebody else into a
    -- game with nobody at this side of it.
    -- With a second tab open, because closing the last one ends the
    -- window, and a window that ends lets go of everything anyway.
    let looking = next (started 1 (next empty' NewTabPressed)) NewTabPressed
    namesOf looking (TabClosePressed "game-1") === [Just "finding game-1"]
    -- A game that already has one is long past that, and what its
    -- closing asks for is letting the opponent go.
    let playing = next oneGame NewTabPressed
    namesOf playing (TabClosePressed "game-1") === [Nothing]

prop_anOpponentFoundForATabThatWentIsGivenBack :: Property
prop_anOpponentFoundForATabThatWentIsGivenBack = property $ do
  (counting', counts) <- evalIO countingSource
  let looking = started 1 (next (startingState counting' asking) NewTabPressed)
      -- The tab closes, and the answer arrives afterwards, which is
      -- what happens when the search was already too far along to
      -- stop.
      gone    = next (next looking NewTabPressed) (TabClosePressed "game-1")
  _           <- ran gone (TabOpened 1 (Right silent))
  (_, back) <- evalIO (readIORef counts)
  -- Given back. Left running, it would be somebody else sitting at a
  -- board waiting for a player who is never going to arrive.
  back === 1
  -- And the window is otherwise unchanged: there is no tab to put it
  -- in and none is made.
  map fst (next gone (TabOpened 1 (Right silent))).openTabs === [2]

-- * Signing in
----------------

-- | A source that says who signed in, and writes down how often it was
-- asked.
--
-- Signing in opens a browser, so being asked twice is two browsers and
-- a second one waiting on a port the first one has.
askedToSignIn :: Either Text Text -> IO (Opponents, IO Int)
askedToSignIn answer = do
  times <- newIORef (0 :: Int)
  pure
    ( source { enter = modifyIORef' times (+ 1) >> pure answer }
    , readIORef times
    )

prop_signingInWaitsAndThenSaysWho :: Property
prop_signingInWaitsAndThenSaysWho = withTests 1 . property $ do
  -- The browser is somewhere else and the window stays up while it is
  -- open, so the waiting is a state of its own rather than a pause.
  let waiting = next empty' (InSetup 1 SignInPressed)
  waiting.signing === SigningIn
  (next waiting (SignedInAs (Right "hikaru"))).signing === SignedIn "hikaru"
  (next waiting (SignedInAs (Left "it was refused"))).signing
    === SignInRefused "it was refused"

prop_signingInIsAskedForOnceHoweverOftenItIsPressed :: Property
prop_signingInIsAskedForOnceHoweverOftenItIsPressed = property $ do
  (asking', count) <- evalIO (askedToSignIn (Right "hikaru"))
  let start = startingState asking' asking
  -- Pressed again while the first one is still going, which is what a
  -- player does when nothing seems to be happening.
  events <- ran start (InSetup 1 SignInPressed)
  let waiting = next start (InSetup 1 SignInPressed)
  -- The second press asks for nothing at all, so there is no second
  -- browser to open and no second port to wait on.
  null (jobsOf (commandOf (update' waiting (InSetup 1 SignInPressed)))) === True
  (next waiting (InSetup 1 SignInPressed)).signing === SigningIn
  times <- evalIO count
  times === 1
  -- And what came back is what the window is told.
  case events of
    [SignedInAs said] -> said === Right "hikaru"
    _                 -> annotate "it answered with something else" >> failure

prop_aWindowStartsSignedOut :: Property
prop_aWindowStartsSignedOut = withTests 1 . property $ do
  -- Nothing is opened until somebody asks. A window that signed in as
  -- it started would open a browser at every launch.
  empty'.signing === SignedOut
  signedIn empty'.signing === False

prop_signingInIsAboutTheWindowAndNotTheTab :: Property
prop_signingInIsAboutTheWindowAndNotTheTab = withTests 1 . property $ do
  -- Every tab shows the same account, so the button on any of them is
  -- the same button, and what it does never reaches the page.
  let two     = next (next empty' NewTabPressed) NewTabPressed
      pressed = next two (InSetup 2 SignInPressed)
  pressed.signing === SigningIn
  chosenIn 1 pressed === Just asking
  chosenIn 2 pressed === Just asking

prop_aSourceMadeOfTwoPicksByWhereTheGameIsPlayed :: Property
prop_aSourceMadeOfTwoPicksByWhereTheGameIsPlayed = property $ do
  asked' <- evalIO (newIORef [])
  let noting what = Opponents
        { open  = \_ -> Right silent <$ modifyIORef' asked' (<> [what])
        , close = \_ -> pure ()
        , enter = pure (Right what)
        }
      both = choosing (noting "computer") (noting "online")
  -- Each place is asked for the games played there and for no others,
  -- and signing in goes to the one place that has accounts.
  _    <- evalIO (both.open asking)
  _    <- evalIO (both.open asking { against = Online })
  _    <- evalIO (both.open asking)
  went <- evalIO (readIORef asked')
  went === ["computer", "online", "computer"]
  name <- evalIO both.enter
  name === Right "online"

-- * The counting at the end
-----------------------------

-- | The game showing, played out until both sides passed and stopped
-- by the other side to be counted.
beingCounted :: Session
beingCounted = foldl onceMore
                     inPlay
                     [ Clicked (Coord 0 0)
                     , Arrived (Played White (Play (Coord 8 8)))
                     , Arrived (Played White Pass)
                     , Passed
                     , Arrived Stopped
                     , Arrived (Removing [Coord 8 8])
                     ]

prop_theTitleSaysToCountTheGame :: Property
prop_theTitleSaysToCountTheGame = withTests 1 . property $ do
  -- Before the game is called over, because a game being counted has
  -- finished as far as the rules here go: both players passed, and
  -- what is left is not a move.
  stateOf beingCounted === "Count the game"
  -- Nothing about counting is written on the board, so what to do with
  -- it has to be written under the title.
  aboutGame beingCounted
    === "nobody  \183  Click a group to mark it dead, then accept"

prop_theSameButtonPassesAndThenAccepts :: Property
prop_theSameButtonPassesAndThenAccepts = withTests 1 . property $ do
  -- They are the same thing at two moments: the one that hands the
  -- game on. Two buttons, with whichever does not apply greyed, would
  -- leave a player looking for the difference between them.
  playable inPlay === True
  canAccept inPlay === False
  playable beingCounted === False
  canAccept beingCounted === True

prop_theBoardIsToldWhichClickItIsTaking :: Property
prop_theBoardIsToldWhichClickItIsTaking = withTests 1 . property $ do
  -- While the game is played a click is about a point, and while it is
  -- counted a click is about a group, so the board cannot work out
  -- which from the position alone.
  settling inPlay === False
  deadIn inPlay === []
  settling beingCounted === True
  deadIn beingCounted === [Coord 8 8]

-- * The clock
---------------

-- | A clock with ten minutes each and Black to play.
onTheClock :: Clock
onTheClock = Clock { black   = Time { seconds = 600, periods = 0, each = 0 }
                   , white   = Time { seconds = 600, periods = 0, each = 0 }
                   , running = Just Black
                   }

-- | Two games open in the window.
twoGames :: State
twoGames =
  next (started 2 (next oneGame NewTabPressed)) (TabOpened 2 (Right silent))

-- | Two games, both timed.
bothTimed :: State
bothTimed = foldl arrive twoGames [1, 2]
  where arrive state tab = next state (InTab tab (Arrived (Ticked onTheClock)))

prop_aSecondGoesByInEveryGameAtOnce :: Property
prop_aSecondGoesByInEveryGameAtOnce = withTests 1 . property $ do
  -- One stream for the window rather than one per tab, because a
  -- second is the same second in every tab. A tab that missed it would
  -- show a clock a second behind the one beside it.
  let onward = next bothTimed Ticking
      left tab = (`clockOf` Black) =<< gameIn tab onward
  left 1 === Just Time { seconds = 599, periods = 0, each = 0 }
  left 2 === Just Time { seconds = 599, periods = 0, each = 0 }

prop_aSecondGoingByLeavesATabThatIsStillAPageAlone :: Property
prop_aSecondGoingByLeavesATabThatIsStillAPageAlone =
  withTests 1 . property $ do
    -- A window can have a game in one tab and a page in another, and
    -- the page has no clock to take anything off.
    let mixed = next (next oneGame NewTabPressed) Ticking
    chosenIn 2 mixed === Just asking
    isJust (gameIn 1 mixed) === True

prop_theSecondsAreOnlyCountedWhileThereIsAClockToCountThem :: Property
prop_theSecondsAreOnlyCountedWhileThereIsAClockToCountThem =
  withTests 1 . property $ do
    -- A window playing GNU Go is never woken. Nothing is timed, so a
    -- stream waking it once a second would be waking it for nothing.
    counts oneGame === False
    counts (next empty' NewTabPressed) === False
    -- One that is playing on a server is woken while a clock is going
    -- down, and stops being woken when it stops.
    counts bothTimed === True
    counts (paused bothTimed) === False
 where
  counts state = "clock" `elem` map subKey (listeners state)
  -- Both games paused, which is what a correspondence game does for
  -- somebody's holiday.
  paused state = foldl stop state [1, 2]
  stop state tab =
    next state (InTab tab (Arrived (Ticked onTheClock { running = Nothing })))

prop_theSubtitleShowsWhatEachSideHasLeft :: Property
prop_theSubtitleShowsWhatEachSideHasLeft = withTests 1 . property $ do
  -- Beside the captures rather than instead of them, and in the same
  -- order every move, so the line can be read at a glance.
  let timed = onceMore inPlay (Arrived (Ticked onTheClock))
  capturesOf timed === "9\215\&9  \183  Black 0  10:00  \183  White 0  10:00"
  -- And a game nobody is timing reads as it always did.
  capturesOf inPlay === "9\215\&9  \183  Black 0  \183  White 0"

prop_theSubtitleCarriesTheNumbers :: Property
prop_theSubtitleCarriesTheNumbers = withTests 1 . property $ do
  -- Who the game is against, and then the numbers. The name is here
  -- as well as on the tab, because a window with one game in it has no
  -- tab bar to put it on.
  aboutGame inPlay === "nobody  \183  " <> capturesOf inPlay
  capturesOf inPlay === "9\215\&9  \183  Black 0  \183  White 0"
  capturesOf (starting (board White 19)) === "19\215\&19  \183  Black 0  \183  White 0"

prop_theSubtitleGivesWayToNews :: Property
prop_theSubtitleGivesWayToNews = withTests 1 . property $ do
  -- A score at the end, a point the rules would not take, and an
  -- engine that has gone all have more to say than the counts do.
  let passed = onceMore inPlay Passed
      scored = onceMore passed (Arrived (Counted "B+2.5"))
  aboutGame scored === "nobody  \183  Result: B+2.5"

  let played = onceMore inPlay (Clicked (Coord 3 3))
      back   = onceMore played (Arrived (Played White (Play (Coord 4 4))))
      onTop  = onceMore back (Clicked (Coord 3 3))
  aboutGame onTop === "nobody  \183  " <> describeIllegal Occupied

  -- An opponent that is gone has no name left to put in front of it,
  -- and why it went is the whole of what there is to say.
  aboutGame (couldNotOpen "no such program" (starting (board Black 9)))
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

prop_aMoveIsSaidAndNothingComesBackThatWay :: Property
prop_aMoveIsSaidAndNothingComesBackThatWay = withTests 1 . property $ do
  -- Playing says two things to the opponent and waits for neither.
  -- What the opponent answers arrives on its stream instead, which is
  -- the subscription below rather than this job.
  answer <- ran oneGame (InTab 1 (Clicked (Coord 3 3)))
  -- Event holds an opponent, which has nothing to print, so what there
  -- is to assert is that there was nothing.
  null answer === True

prop_everyGameIsListenedToAndNothingElseIs :: Property
prop_everyGameIsListenedToAndNothingElseIs = withTests 1 . property $ do
  let two     = next (started 2 (next oneGame NewTabPressed))
                     (TabOpened 2 (Right silent))
      opening = next two NewTabPressed
      broken  = next opening (InTab 1 (Arrived (Lost "gone")))
  -- One stream for each game that has an opponent.
  length (listeners empty') === 0
  length (listeners oneGame) === 1
  length (listeners two) === 2
  -- The third tab is still on its page, so there is nothing to listen
  -- to yet, and a game whose opponent has gone is listened to no
  -- longer.
  length (listeners opening) === 2
  length (listeners broken) === 1

prop_twoGamesAreTwoStreamsAndNotOne :: Property
prop_twoGamesAreTwoStreamsAndNotOne = withTests 1 . property $ do
  -- A name is shared by everything the loop runs, so two games under
  -- one name would be one game listened to twice.
  let two   = next (started 2 (next oneGame NewTabPressed))
                   (TabOpened 2 (Right silent))
      names = map subKey (listeners two)
  length names === 2
  names === List.nub names

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
        Setup { against  = asking.against
              , size     = asking.size
              , human    = White
              , strength = asking.strength
              , handicap = asking.handicap
              , speed    = asking.speed
              , ranked   = asking.ranked
              , handicapping = asking.handicapping
              , versus   = asking.versus
              }
      built = application source playingWhite
  (initialState built).opening.human === White
  case update built (initialState built) NewTabPressed of
    Transition state' _ -> map fst state'.openTabs === [1]
    Exit                -> annotate "it exited" >> failure

-- * The menu
---------------

prop_theMenuOffersTheSameItemsEveryTime :: Property
prop_theMenuOffersTheSameItemsEveryTime = withTests 1 . property $ do
  -- Greyed rather than gone, so the menu does not change shape under
  -- somebody who has opened it before.
  map (.name) (choices (MenuProps { canResign = True }))
    === map (.name) (choices (MenuProps { canResign = False }))
  map (.name) (choices (MenuProps { canResign = True }))
    === ["new-game", "resign"]

prop_aGameCanBeGivenUpAndAPageCannot :: Property
prop_aGameCanBeGivenUpAndAPageCannot = withTests 1 . property $ do
  map (.usable) (choices (MenuProps { canResign = True })) === [True, True]
  map (.usable) (choices (MenuProps { canResign = False })) === [True, False]

prop_theMenuResignsTheGameShowing :: Property
prop_theMenuResignsTheGameShowing = withTests 1 . property $ do
  -- The menu acts on whatever is showing, which is what a menu does.
  let two      = next (started 2 (next oneGame NewTabPressed))
                      (TabOpened 2 (Right silent))
      onFirst  = next two (TabSelected "game-1")
      resigned = next onFirst ResignChosen
  fmap (finished . (.game)) (gameIn 1 resigned) === Just True
  fmap (finished . (.game)) (gameIn 2 resigned) === Just False

prop_resigningWithAPageShowingDoesNothing :: Property
prop_resigningWithAPageShowingDoesNothing = withTests 1 . property $ do
  let opening = next oneGame NewTabPressed
      asked'  = next opening ResignChosen
  -- The item is greyed, and the window drops it even so.
  map fst asked'.openTabs === [1, 2]
  fmap (finished . (.game)) (gameIn 1 asked') === Just False
  -- And a window with nothing open at all.
  map fst (next empty' ResignChosen).openTabs === []

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