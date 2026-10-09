-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | "Stones.Opponent.GnuGo" against an engine that is not GNU Go.
--
-- The engine is a shell script, so it can be made to refuse a command,
-- to answer with something that is not a move, or not to be there at
-- all. GNU Go will not do any of those to order, and each of them is a
-- thing the window has to say something sensible about.
module FakeEngineTest
  ( tests
  )
where

import           Control.Exception              ( bracket )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Hedgehog
import           System.Directory
import           System.IO                      ( hClose
                                                , hPutStr
                                                , openTempFile
                                                )

import           Go.Types
import           Pipes                           ( (>->) )
import qualified Pipes.Prelude                 as Pipes

import           Stones.Opponent
import           Stones.Setup                   ( Against(..)
                                                , Handicapping(..)
                                                , Setup(..)
                                                , Speed(..)
                                                , Strength(..)
                                                )
import           Stones.Log                     ( quiet )
import qualified Stones.Opponent.GnuGo           as GnuGo

-- | Write a shell script that behaves like an engine, and run
-- something with the path to it.
--
-- The file is opened rather than named, because the tests run
-- alongside each other and a name picked in advance is a name two of
-- them can pick. The name given here is only so that a file left
-- behind by a test that died says which test left it.
withEngineNamed :: String -> String -> (FilePath -> IO a) -> IO a
withEngineNamed name body use = do
  dir <- getTemporaryDirectory
  bracket (write dir) removeFile use
 where
  write dir = do
    (path, handle) <- openTempFile dir ("stones-fake-" <> name <> "-.sh")
    hPutStr handle ("#!/bin/sh\n" <> body)
    hClose handle
    setPermissions
      path
      (setOwnerExecutable True (setOwnerReadable True emptyPermissions))
    pure path

-- | An engine that plays D4 every time and counts the game as a win
-- for Black.
obliging :: String
obliging = unlines
  [ "while read -r line; do"
  , "  case \"$line\" in"
  , "    *genmove*) printf '= D4\\n\\n' ;;"
  , "    *final_score*) printf '= B+2.5\\n\\n' ;;"
  , "    *) printf '=\\n\\n' ;;"
  , "  esac"
  , "done"
  ]

-- | An engine that refuses everything.
refusing :: String
refusing = "while read -r line; do printf '? not today\\n\\n'; done"

-- | An engine that answers with something that is not a move.
babbling :: String
babbling = "while read -r line; do printf '= nonsense\\n\\n'; done"

prop_anEngineThatIsNotThereIsReported :: Property
prop_anEngineThatIsNotThereIsReported = withTests 1 . property $ do
  opened <- evalIO (GnuGo.open quiet "/nonexistent-engine" (board 9))
  case opened of
    Right _  -> annotate "it started something" >> failure
    Left why -> do
      assert (Text.isPrefixOf "Could not start /nonexistent-engine" why)
  probed <- evalIO (GnuGo.probe quiet "/nonexistent-engine" (board 9))
  case probed of
    Right () -> annotate "it found something" >> failure
    Left _   -> success

prop_anEngineThatRefusesTheFirstCommandDoesNotStart :: Property
prop_anEngineThatRefusesTheFirstCommandDoesNotStart =
  withTests 1 . property $ do
    opened <- evalIO . withEngineNamed "refusing" refusing $ \path ->
      GnuGo.open quiet path (board 9)
    case opened of
      Right _  -> annotate "it started" >> failure
      Left why -> assert (Text.isInfixOf "not today" why)

prop_anObligingEngineCanBePlayedAgainst :: Property
prop_anObligingEngineCanBePlayedAgainst = withTests 1 . property $ do
  answers <- evalIO . withEngineNamed "obliging" obliging $ \path ->
    GnuGo.open quiet path (board 9) >>= \case
      Left  why    -> pure (Left why)
      Right them -> do
        told    <- them.tell (Moved (Play (Coord 3 3)))
        passed  <- them.tell (Moved Pass)
        moved   <- asking them Asked
        nothing <- them.tell (TookBack 0)
        back    <- them.tell (TookBack 2)
        scored  <- asking them Counting
        quiet   <- them.tell (Said "hello")
        them.leave
        pure (Right (them.name, told, passed, moved, nothing, back, scored, quiet))
  case answers of
    Left why -> annotate (Text.unpack why) >> failure
    Right (name, told, passed, moved, nothing, back, scored, quiet) -> do
      name === "GNU Go"
      told === Right ()
      passed === Right ()
      -- D4 on a 9x9 board, counted from the top. The player is Black
      -- here, so Asked asks White, and that is the colour it comes
      -- back under.
      moved === Right [Played White (Play (Coord 3 5))]
      nothing === Right ()
      back === Right ()
      scored === Right [Counted "B+2.5"]
      -- GNU Go has nobody to say it to, and takes it without a word.
      quiet === Right ()

prop_anEngineThatAnswersWithNonsenseIsReported :: Property
prop_anEngineThatAnswersWithNonsenseIsReported = withTests 1 . property $ do
  answer <- evalIO . withEngineNamed "babbling" babbling $ \path ->
    GnuGo.open quiet path (board 9) >>= \case
      Left  why    -> pure (Left why)
      Right them -> do
        moved <- asking them Asked
        them.leave
        pure (Right moved)
  -- A move it cannot read is not a refusal. It is the other side
  -- going, because there is nothing else it can be.
  case answer of
    Right (Right [Lost why]) ->
      why === "The engine answered with \"nonsense\"."
    other -> annotateShow (fmap (const ()) other) >> failure

prop_anEngineThatGoesAwayMidGameIsReported :: Property
prop_anEngineThatGoesAwayMidGameIsReported = withTests 1 . property $ do
  -- It answers the two commands that set the board up, and then goes.
  let brief = unlines
        [ "read -r line; printf '=\\n\\n'"
        , "read -r line; printf '=\\n\\n'"
        , "exit 0"
        ]
  answer <- evalIO . withEngineNamed "brief" brief $ \path ->
    GnuGo.open quiet path (board 9) >>= \case
      Left  why    -> pure (Left why)
      Right them -> do
        moved <- asking them Asked
        them.leave
        pure (Right moved)
  -- A pipe that closes is not a refusal either. It is the other side
  -- going, and it arrives the same way a refused move does.
  case answer of
    Right (Right [Lost why]) ->
      assert (Text.isPrefixOf "Lost the engine" why)
    other -> annotateShow (fmap (const ()) other) >> failure

prop_severalOpponentsAtOnceAndAllOfThemStopped :: Property
prop_severalOpponentsAtOnceAndAllOfThemStopped = withTests 1 . property $ do
  -- A window with three tabs has three of these, and what is still
  -- running when the window closes is stopped with it.
  answers <- evalIO . withEngineNamed "several" obliging $ \path ->
    GnuGo.withGnuGo quiet path $ \opponents -> do
      one   <- opponents.open (board 9)
      two   <- opponents.open (board 13)
      three <- opponents.open (board 19)
      -- One of them is let go by hand, the way a tab closing does it.
      case one of
        Right engine -> opponents.close engine
        Left  _      -> pure ()
      pure (map named [one, two, three])
  answers === [Just "GNU Go", Just "GNU Go", Just "GNU Go"]
 where
  named :: Either Text Opponent -> Maybe Text
  named (Right engine) = Just engine.name
  named (Left  _     ) = Nothing

prop_theLevelsAreTheOnesGnuGoTakes :: Property
prop_theLevelsAreTheOnesGnuGoTakes = withTests 1 . property $ do
  GnuGo.levelRange === (1, 10)
  GnuGo.defaultLevel === GnuGo.Level 10
  assert (GnuGo.Level 1 < GnuGo.defaultLevel)

-- | Silence the unused import warning for Text, which the bodies above
-- use through Text.isPrefixOf and friends.
_unused :: Text
_unused = ""

tests :: Group
tests = $$(discover)

-- | A game on a board this wide, as Black against an opponent that is
-- trying.
board :: Int -> Setup
board n = Setup { against = Computer, size = n, human = Black, strength = Strength 10, handicap = 0, speed = Live, ranked = False, handicapping = Standard, versus = "" }

-- | Say one thing to an opponent, then read what came back off its
-- stream.
asking :: Opponent -> Told -> IO (Either Text [Heard])
asking them this = them.tell this >>= \said -> case said of
  Left  why -> pure (Left why)
  Right ()  -> Right <$> Pipes.toListM (them.hear >-> Pipes.take 1)
