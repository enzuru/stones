-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Talking to a real GNU Go.
--
-- This is the one test that starts a process. It is not a property:
-- there is one engine, and what is being checked is that the two
-- programs understand each other, which either happens or does not.
--
-- A machine with no GNU Go on it skips the test and says so, rather
-- than failing, so that the suite still runs on one.
module GtpTest
  ( run
  )
where

import           Data.IORef
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           System.Directory               ( findExecutable )

import           Go.Types
import           Go.Vertex
import           Pipes                           ( (>->) )
import qualified Pipes.Prelude                 as Pipes

import           Stones.Opponent
import           Stones.Setup                   ( Against(..)
                                                , Setup(..)
                                                , Speed(..)
                                                , Strength(..)
                                                )
import qualified Stones.Opponent.GnuGo           as GnuGo

-- | Run the checks. The answer is whether they passed.
run :: IO Bool
run = findExecutable "gnugo" >>= \found -> case found of
  Nothing -> do
    Text.putStrLn "GtpTest: no gnugo on this machine, skipping."
    pure True
  Just program -> do
    Text.putStrLn "GtpTest: talking to gnugo."
    -- A program that is not there is a line on the terminal rather
    -- than a window with a tab that never starts.
    missing <- GnuGo.probe "/nonexistent-gnugo" (board 9)
    present <- GnuGo.probe program (board 9)
    opened  <- GnuGo.open program (board 9)
    case (missing, present, opened) of
      (Right (), _, _) -> failed "it found an engine that is not there"
      (_, Left why, _) -> failed ("it could not find gnugo: " <> why)
      (_, _, Left problem) -> failed ("could not start it: " <> problem)
      (_, _, Right them) -> do
        failures <- newIORef (0 :: Int)
        let check name action = action >>= \outcome -> case outcome of
              Right () -> pure ()
              Left why -> do
                modifyIORef' failures (+ 1)
                Text.putStrLn ("GtpTest: " <> name <> ": " <> why)
        check "a move it is told about"
              (them.tell (Moved (Play (Coord 3 3))))
        -- What it answers arrives on its stream rather than in the
        -- answer, which is where a game on a server would put it.
        check "a move of its own" $ asking them Asked >>= \answer ->
          pure $ case answer of
            Left  why              -> Left why
            Right [Played White m] -> case m of
              Pass   -> Left "it passed on an empty board"
              Resign -> Left "it resigned on an empty board"
              Play coord
                | coord == Coord 3 3 -> Left "it played on top of a stone"
                | otherwise -> case fromVertex 9 =<< toVertex 9 coord of
                  Just same | same == coord -> Right ()
                  _ -> Left "its move is not a point on a 9x9 board"
            Right heard -> Left (Text.pack (show heard))
        check "taking a move back" (them.tell (TookBack 1))
        check "a score" $ asking them Counting >>= \answer ->
          pure $ case answer of
            Left  why              -> Left why
            Right [Counted result] -> if Text.null result
              then Left "it gave an empty score"
              else Right ()
            Right heard -> Left (Text.pack (show heard))
        -- A handicap game, where the stones go down before anybody
        -- moves and the position arrives the way a server's would.
        check "a handicap" $ GnuGo.open program (board 19) { handicap = 4 }
          >>= \started -> case started of
                Left  why   -> pure (Left why)
                Right other -> do
                  heard <- Pipes.toListM (other.hear >-> Pipes.take 1)
                  other.leave
                  pure $ case heard of
                    [Began opening]
                      | length opening.stones /= 4
                      -> Left "it put down the wrong number of stones"
                      | opening.opens /= White
                      -> Left "it left Black to move in a handicap game"
                      | any ((/= Black) . fst) opening.stones
                      -> Left "it gave the handicap to the wrong colour"
                      | otherwise
                      -> Right ()
                    other' -> Left (Text.pack (show other'))
        -- Two opponents at once, which is what a window with two tabs
        -- in it has, each holding a board of its own.
        check "two engines at once" $ GnuGo.withGnuGo program
          $ \opponents -> do
              first'  <- opponents.open (board 9)
              second' <- opponents.open (board 19)
              case (first', second') of
                (Right one, Right other) -> do
                  said <- one.tell (Moved (Play (Coord 0 0)))
                  -- A19 is a point on the second board and not on the
                  -- first, so the two are not the same board.
                  far  <- other.tell (Moved (Play (Coord 18 18)))
                  opponents.close one
                  opponents.close other
                  pure (said >> far)
                _ -> pure (Left "could not start two")
        them.leave
        count <- readIORef failures
        if count == 0
          then True <$ Text.putStrLn "GtpTest: passed."
          else failed (Text.pack (show count) <> " checks failed")
 where
  -- A game on a board this wide, as Black, against an opponent that is
  -- trying. Black here means Asked asks White, which is what the
  -- checks above expect.
  board n =
    Setup { against = Computer, size = n, human = Black, strength = Fierce, handicap = 0, speed = Live, ranked = False, versus = "" }

  -- Say one thing, then read what came back off the stream. Only the
  -- things that answer are asked this way.
  asking :: Opponent -> Told -> IO (Either Text.Text [Heard])
  asking them this = them.tell this >>= \said -> case said of
    Left  why -> pure (Left why)
    Right ()  -> Right <$> Pipes.toListM (them.hear >-> Pipes.take 1)

  failed why = False <$ Text.putStrLn ("GtpTest: failed, " <> why)
