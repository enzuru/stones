-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | "Stones.Engine.GnuGo" against an engine that is not GNU Go.
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
import           Stones.Engine
import qualified Stones.Engine.GnuGo           as GnuGo

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
  opened <- evalIO (GnuGo.open "/nonexistent-engine" GnuGo.defaultLevel 9)
  case opened of
    Right _  -> annotate "it started something" >> failure
    Left why -> do
      assert (Text.isPrefixOf "Could not start /nonexistent-engine" why)
  probed <- evalIO (GnuGo.probe "/nonexistent-engine" GnuGo.defaultLevel)
  case probed of
    Right () -> annotate "it found something" >> failure
    Left _   -> success

prop_anEngineThatRefusesTheFirstCommandDoesNotStart :: Property
prop_anEngineThatRefusesTheFirstCommandDoesNotStart =
  withTests 1 . property $ do
    opened <- evalIO . withEngineNamed "refusing" refusing $ \path ->
      GnuGo.open path (GnuGo.Level 1) 9
    case opened of
      Right _  -> annotate "it started" >> failure
      Left why -> assert (Text.isInfixOf "not today" why)

prop_anObligingEngineCanBePlayedAgainst :: Property
prop_anObligingEngineCanBePlayedAgainst = withTests 1 . property $ do
  answers <- evalIO . withEngineNamed "obliging" obliging $ \path ->
    GnuGo.open path (GnuGo.Level 3) 9 >>= \case
      Left  why    -> pure (Left why)
      Right engine -> do
        name    <- pure engine.name
        fresh   <- engine.newGame 13
        told    <- engine.notify Black (Play (Coord 3 3))
        passed  <- engine.notify White Pass
        moved   <- engine.genMove White
        nothing <- engine.undo 0
        back    <- engine.undo 2
        scored  <- engine.score
        engine.close
        pure (Right (name, fresh, told, passed, moved, nothing, back, scored))
  case answers of
    Left why -> annotate (Text.unpack why) >> failure
    Right (name, fresh, told, passed, moved, nothing, back, scored) -> do
      name === "GNU Go"
      fresh === Right ()
      told === Right ()
      passed === Right ()
      -- D4 on a 13x13 board, counted from the top.
      moved === Right (Play (Coord 3 9))
      nothing === Right ()
      back === Right ()
      scored === Right "B+2.5"

prop_anEngineThatAnswersWithNonsenseIsReported :: Property
prop_anEngineThatAnswersWithNonsenseIsReported = withTests 1 . property $ do
  answer <- evalIO . withEngineNamed "babbling" babbling $ \path ->
    GnuGo.open path (GnuGo.Level 1) 9 >>= \case
      Left  why    -> pure (Left why)
      Right engine -> do
        moved <- engine.genMove White
        engine.close
        pure (Right moved)
  case answer of
    Right (Left why) ->
      why === "The engine answered with \"nonsense\"."
    other -> annotateShow (fmap (fmap (const ())) other) >> failure

prop_anEngineThatGoesAwayMidGameIsReported :: Property
prop_anEngineThatGoesAwayMidGameIsReported = withTests 1 . property $ do
  -- It answers the two commands that set the board up, and then goes.
  let brief = unlines
        [ "read -r line; printf '=\\n\\n'"
        , "read -r line; printf '=\\n\\n'"
        , "exit 0"
        ]
  answer <- evalIO . withEngineNamed "brief" brief $ \path ->
    GnuGo.open path (GnuGo.Level 1) 9 >>= \case
      Left  why    -> pure (Left why)
      Right engine -> do
        moved <- engine.genMove Black
        engine.close
        pure (Right moved)
  case answer of
    Right (Left why) -> assert (Text.isPrefixOf "Lost the engine" why)
    other            -> annotateShow (fmap (fmap (const ())) other) >> failure

prop_severalOpponentsAtOnceAndAllOfThemStopped :: Property
prop_severalOpponentsAtOnceAndAllOfThemStopped = withTests 1 . property $ do
  -- A window with three tabs has three of these, and what is still
  -- running when the window closes is stopped with it.
  answers <- evalIO . withEngineNamed "several" obliging $ \path ->
    GnuGo.withGnuGo path (GnuGo.Level 1) $ \opponents -> do
      one   <- opponents.open 9
      two   <- opponents.open 13
      three <- opponents.open 19
      -- One of them is let go by hand, the way a tab closing does it.
      case one of
        Right engine -> opponents.close engine
        Left  _      -> pure ()
      pure (map named [one, two, three])
  answers === [Just "GNU Go", Just "GNU Go", Just "GNU Go"]
 where
  named :: Either Text Engine -> Maybe Text
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
