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
import           Stones.Engine
import qualified Stones.Engine.GnuGo           as GnuGo

-- | Run the checks. The answer is whether they passed.
run :: IO Bool
run = findExecutable "gnugo" >>= \found -> case found of
  Nothing -> do
    Text.putStrLn "GtpTest: no gnugo on this machine, skipping."
    pure True
  Just program -> do
    Text.putStrLn "GtpTest: talking to gnugo."
    opened <- GnuGo.open program (GnuGo.Level 1) 9
    case opened of
      Left problem -> failed ("could not start it: " <> problem)
      Right engine -> do
        failures <- newIORef (0 :: Int)
        let check name action = action >>= \outcome -> case outcome of
              Right () -> pure ()
              Left why -> do
                modifyIORef' failures (+ 1)
                Text.putStrLn ("GtpTest: " <> name <> ": " <> why)
        check "a new board" (engineNewGame engine 9)
        check "a move it is told about"
              (engineNotify engine Black (Play (Coord 3 3)))
        check "a move of its own" $ engineGenMove engine White >>= \answer ->
          pure $ case answer of
            Left  why  -> Left why
            Right Pass -> Left "it passed on an empty board"
            Right Resign -> Left "it resigned on an empty board"
            Right (Play coord)
              | coord == Coord 3 3 -> Left "it played on top of a stone"
              | otherwise -> case fromVertex 9 (toVertex 9 coord) of
                Just same | same == coord -> Right ()
                _ -> Left "its move is not a point on a 9x9 board"
        check "taking a move back" (engineUndo engine 1)
        check "a score" $ engineScore engine >>= \answer -> pure $ case answer of
          Left  why    -> Left why
          Right result -> if Text.null result
            then Left "it gave an empty score"
            else Right ()
        engineClose engine
        count <- readIORef failures
        if count == 0
          then True <$ Text.putStrLn "GtpTest: passed."
          else failed (Text.pack (show count) <> " checks failed")
 where
  failed why = False <$ Text.putStrLn ("GtpTest: failed, " <> why)
