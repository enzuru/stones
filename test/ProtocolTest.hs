{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Reading answers off the Go Text Protocol, including the answers a
-- real engine gives only when something has gone wrong.
--
-- The engine here is a shell script. The protocol is lines in and
-- lines out, so a program that prints the lines a test wants is an
-- engine as far as this code is concerned, and it can be made to
-- refuse, to chatter, or to die, which GNU Go will not do to order.
module ProtocolTest
  ( tests
  )
where

import           Control.Exception              ( bracket )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Hedgehog

import qualified Stones.Engine.Gtp             as Gtp

-- | An engine that answers every command with these lines, over and
-- over, and a blank line after each answer.
answering :: [Text] -> (Gtp.Gtp -> IO a) -> IO a
answering reply = withEngine
  ("while read -r line; do printf '%s\\n\\n'; done" `with` Text.intercalate
    "\\n"
    reply
  )
  where with script body = Text.unpack (Text.replace "%s" body (Text.pack script))

-- | Run something with an engine made from this shell script.
withEngine :: String -> (Gtp.Gtp -> IO a) -> IO a
withEngine script = bracket (Gtp.start "sh" ["-c", script]) Gtp.stop

prop_readsAPlainAnswer :: Property
prop_readsAPlainAnswer = withTests 1 . property $ do
  answer <- evalIO (answering ["= D4"] (`Gtp.command` "genmove black"))
  answer === Right "D4"

prop_readsAnAnswerWithNothingInIt :: Property
prop_readsAnAnswerWithNothingInIt = withTests 1 . property $ do
  -- What a command that only does something answers with.
  answer <- evalIO (answering ["="] (`Gtp.command` "clear_board"))
  answer === Right ""

prop_dropsTheIdentifierTheEngineEchoes :: Property
prop_dropsTheIdentifierTheEngineEchoes = withTests 1 . property $ do
  answer <- evalIO (answering ["=7 D4"] (`Gtp.command` "genmove black"))
  answer === Right "D4"

prop_readsAnAnswerOfSeveralLines :: Property
prop_readsAnAnswerOfSeveralLines = withTests 1 . property $ do
  -- What showboard answers with.
  answer <- evalIO (answering ["= one", "two", "three"] (`Gtp.command` "showboard"))
  answer === Right "one\ntwo\nthree"

prop_aRefusalIsARefusalAndSaysWhy :: Property
prop_aRefusalIsARefusalAndSaysWhy = withTests 1 . property $ do
  answer <- evalIO (answering ["? illegal move"] (`Gtp.command` "play black D4"))
  answer === Left (Gtp.Refused "illegal move")

prop_aRefusalWithNoReasonStillSaysSomething :: Property
prop_aRefusalWithNoReasonStillSaysSomething = withTests 1 . property $ do
  answer <- evalIO (answering ["?"] (`Gtp.command` "play black D4"))
  answer === Left (Gtp.Refused "no reason given")

prop_linesBeforeTheAnswerAreNotTheAnswer :: Property
prop_linesBeforeTheAnswerAreNotTheAnswer = withTests 1 . property $ do
  -- A program that prints something of its own before it answers.
  answer <- evalIO
    (answering ["chatter", "more chatter", "= D4"] (`Gtp.command` "genmove black"))
  answer === Right "D4"

prop_anEngineThatHasGoneIsReportedAsGone :: Property
prop_anEngineThatHasGoneIsReportedAsGone = withTests 1 . property $ do
  answer <- evalIO (withEngine "exit 0" (`Gtp.command` "genmove black"))
  case answer of
    Left (Gtp.Broken _) -> success
    other               -> annotateShow other >> failure

prop_anEngineThatDiesMidWayIsReportedAsGone :: Property
prop_anEngineThatDiesMidWayIsReportedAsGone = withTests 1 . property $ do
  -- It answers the first command and then goes.
  answer <- evalIO $ withEngine "read -r line; printf '= fine\\n\\n'; exit 0" $ \gtp -> do
    first' <- Gtp.command gtp "boardsize 9"
    second <- Gtp.command gtp "genmove black"
    pure (first', second)
  case answer of
    (Right "fine", Left (Gtp.Broken _)) -> success
    other                               -> annotateShow other >> failure

prop_commandUnderscoreKeepsOnlyTheFailure :: Property
prop_commandUnderscoreKeepsOnlyTheFailure = withTests 1 . property $ do
  good <- evalIO (answering ["= D4"] (`Gtp.command_` "genmove black"))
  good === Right ()
  bad <- evalIO (answering ["? no"] (`Gtp.command_` "play black D4"))
  bad === Left (Gtp.Refused "no")

prop_anEngineIsRunningUntilItIsStopped :: Property
prop_anEngineIsRunningUntilItIsStopped = withTests 1 . property $ do
  (before, after) <- evalIO $ do
    gtp    <- Gtp.start "sh" ["-c", "while read -r line; do printf '=\\n\\n'; done"]
    before <- Gtp.alive gtp
    Gtp.stop gtp
    after <- Gtp.alive gtp
    pure (before, after)
  before === True
  after === False

prop_stoppingAnEngineTwiceDoesNothingTheSecondTime :: Property
prop_stoppingAnEngineTwiceDoesNothingTheSecondTime = withTests 1 . property $ do
  -- A game's opponent is stopped when its tab closes and again when
  -- the window closes. Waiting twice on one process is an error, so
  -- the second stop has to be the one that does nothing.
  evalIO $ do
    gtp <- Gtp.start "sh" ["-c", "while read -r line; do printf '=\\n\\n'; done"]
    Gtp.stop gtp
    Gtp.stop gtp
    Gtp.stop gtp
  success

prop_aRefusalAndABreakReadDifferently :: Property
prop_aRefusalAndABreakReadDifferently = withTests 1 . property $ do
  Gtp.describeGtpError (Gtp.Refused "no")
    === "The engine refused the move: no"
  Gtp.describeGtpError (Gtp.Broken "gone") === "Lost the engine: gone"

tests :: Group
tests = $$(discover)
