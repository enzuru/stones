{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The tabs: which games are open, which one is showing, and where
-- each answer belongs.
module AppTest
  ( tests
  )
where

import           Hedgehog
import qualified Data.Vector                   as Vector

import           GI.Gtk.Declarative.App.Simple  ( Transition(..) )

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

-- | A source of opponents that hands out the same silent one. Nothing
-- here runs: 'update'' is a function, and the commands it answers with
-- are not run by these tests.
source :: Opponents
source = Opponents { openOpponent  = \_ -> pure (Right silent)
                   , closeOpponent = \_ -> pure ()
                   }

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
  fmap sessionMessage (gameIn 2 failed) === Just "no such program"
  fmap playable (gameIn 2 failed) === Just False
  fmap playable (gameIn 1 failed) === Just True

tests :: Group
tests = $$(discover)
