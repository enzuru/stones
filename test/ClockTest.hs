-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later


-- | Clocks: the seconds coming off one, and how one reads.
--
-- A game on a server has a clock running whether this program is
-- watching it or not, and the server only says where it is when
-- somebody moves. The seconds in between are counted here, so this is
-- the one part of a timed game that is this program's own arithmetic
-- rather than the server's.
module ClockTest
  ( tests
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Go.Types
import           Stones.Opponent                ( Clock(..)
                                                , Time(..)
                                                , describeTime
                                                , ticking
                                                )

-- | A time somebody might have left.
time :: Gen Time
time = do
  seconds <- Gen.int (Range.linear 0 7200)
  periods <- Gen.int (Range.linear 0 10)
  each    <- Gen.int (Range.linear 0 120)
  pure Time { seconds = seconds, periods = periods, each = each }

-- | A clock, with either side to play or neither.
clock :: Gen Clock
clock = do
  black   <- time
  white   <- time
  running <- Gen.element ([Nothing, Just Black, Just White] :: [Maybe Color])
  pure Clock { black = black, white = white, running = running }

-- | Everything one side has left, the short periods included.
allOf :: Time -> Int
allOf t = t.seconds + t.periods * t.each

prop_aSecondComesOffWhoeverIsToPlayAndOffNobodyElse :: Property
prop_aSecondComesOffWhoeverIsToPlayAndOffNobodyElse = property $ do
  before <- forAll clock
  let after = ticking before
  -- The side that is not to play is the side that was not touched,
  -- whichever side that is.
  case before.running of
    Nothing    -> after === before
    Just Black -> after.white === before.white
    Just White -> after.black === before.black

prop_nobodysClockGoesDownWhenNobodyIsToPlay :: Property
prop_nobodysClockGoesDownWhenNobodyIsToPlay = property $ do
  -- Before the game starts, after it ends, and while it is paused.
  stopped <- forAll ((\c -> c { running = Nothing }) <$> clock)
  ticking stopped === stopped

prop_aClockNeverGoesBackwards :: Property
prop_aClockNeverGoesBackwards = property $ do
  before <- forAll clock
  let after = ticking before
  -- A second at a time, and never the other way. A clock cannot hand
  -- time back, whatever it was set to.
  assert (allOf after.black <= allOf before.black)
  assert (allOf after.white <= allOf before.white)
  assert (after.black.seconds >= 0 && after.white.seconds >= 0)
  assert (after.black.periods >= 0 && after.white.periods >= 0)

prop_theShortPeriodsComeAfterTheTimeToThinkIn :: Property
prop_theShortPeriodsComeAfterTheTimeToThinkIn = property $ do
  -- The time to think in has gone and there are periods behind it, so
  -- the next second is the first of the next period.
  periods <- forAll (Gen.int (Range.linear 1 10))
  each    <- forAll (Gen.int (Range.linear 1 120))
  let out  = Time { seconds = 0, periods = periods, each = each }
      next = ticking Clock { black = out, white = out, running = Just Black }
  next.black.periods === periods - 1
  next.black.seconds === each - 1

prop_aClockThatHasRunRightOutStaysRunOut :: Property
prop_aClockThatHasRunRightOutStaysRunOut = property $ do
  -- Nothing left and nothing behind it. The server is the one that
  -- ends a game on time, so this sits at nothing rather than going
  -- below it or wrapping round.
  let gone  = Time { seconds = 0, periods = 0, each = 30 }
      still = Clock { black = gone, white = gone, running = Just Black }
  ticking still === still
  ticking (ticking still) === still

prop_aClockReadsAsMinutesAndSeconds :: Property
prop_aClockReadsAsMinutesAndSeconds = withTests 1 . property $ do
  describeTime Time { seconds = 754, periods = 0, each = 0 } === "12:34"
  describeTime Time { seconds = 60, periods = 0, each = 0 } === "1:00"
  describeTime Time { seconds = 5, periods = 0, each = 0 } === "0:05"
  describeTime Time { seconds = 0, periods = 0, each = 0 } === "0:00"
  -- In byo-yomi, what is running and how many are still to come.
  describeTime Time { seconds = 30, periods = 5, each = 30 } === "0:30 (\215\&5)"

prop_theSecondsAreAlwaysTwoDigits :: Property
prop_theSecondsAreAlwaysTwoDigits = property $ do
  -- A clock with a single digit in it jumps about as it counts down,
  -- because the line it sits on gets shorter. Padding is what stops
  -- that, along with the numeric style class on the label.
  t <- forAll time
  let (minutes, rest) = Text.breakOn ":" (Text.takeWhile (/= ' ') (describeTime t))
      shown           = Text.drop 1 rest
  Text.length shown === 2
  assert (Text.all (`elem` ("0123456789" :: String)) shown)
  assert (Text.all (`elem` ("0123456789" :: String)) minutes)
  assert (not (Text.null minutes))
  -- The two halves are the time, back again.
  read (Text.unpack minutes) * 60 + read (Text.unpack shown) === t.seconds

-- | Silence the unused import warning for Text, which the bodies use
-- and the signatures do not name.
_unused :: Text
_unused = ""

tests :: Group
tests = $$(discover)
