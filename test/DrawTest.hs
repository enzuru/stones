-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | What the board draws, and what it does with a click.
--
-- Cairo draws onto a surface, and a surface in memory is as good as
-- one on a screen, so the drawing is checked by drawing it and looking
-- at the pixels. Nothing here needs a display or a widget: the whole
-- of "Stones.Goban" that decides anything is a function of its
-- arguments.
module DrawTest
  ( tests
  )
where

import           Data.ByteString                ( ByteString )
import           Data.IORef
import qualified Data.ByteString               as ByteString
import qualified GI.Cairo.Render               as Cairo
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Go.Board
import           Go.Types
import           Stones.Goban
import           Stones.Goban.Geometry

-- | A board of this width with these stones on it.
boardOf :: Int -> [(Color, (Int, Int))] -> Board
boardOf n = foldl put (emptyBoard n)
 where
  put board (color, (x, y)) = case place color (Coord x y) board of
    Right placement -> placedBoard placement
    Left  _         -> board

-- | An ordinary board to draw: a few stones, coordinates on, and the
-- player to move.
shown :: GobanProps
shown = GobanProps { gobanBoard = boardOf 9 [(Black, (2, 2)), (White, (4, 4))]
                   , gobanLast        = Nothing
                   , gobanHover       = Just Black
                   , gobanCoordinates = True
                   }

-- | Draw a board onto a surface this size, and answer with its pixels.
--
-- The bytes are copied out, because the surface they belong to is
-- freed as soon as this returns.
pixels :: GobanProps -> Bool -> Maybe Coord -> Int -> Int -> IO ByteString
pixels props dark hovered width height =
  Cairo.withImageSurface Cairo.FormatARGB32 width height $ \surface -> do
    Cairo.renderWith
      surface
      (draw props dark hovered (fromIntegral width) (fromIntegral height))
    Cairo.surfaceFlush surface
    ByteString.copy <$> Cairo.imageSurfaceGetData surface

-- | The ordinary drawing of a board, at a size a window would give it.
drawn :: GobanProps -> IO ByteString
drawn props = pixels props False Nothing 400 400

-- | Is there anything on this surface at all?
blank :: ByteString -> Bool
blank = ByteString.all (== 0)

prop_everyBoardSizeDrawsSomething :: Property
prop_everyBoardSizeDrawsSomething = property $ do
  n       <- forAll (Gen.element [9, 13, 19])
  surface <- evalIO (drawn shown { gobanBoard = emptyBoard n })
  assert (not (blank surface))

prop_aStoneChangesThePicture :: Property
prop_aStoneChangesThePicture = withTests 1 . property $ do
  bare    <- evalIO (drawn shown { gobanBoard = emptyBoard 9 })
  stoned  <- evalIO (drawn shown)
  assert (bare /= stoned)

prop_theLastStoneIsMarked :: Property
prop_theLastStoneIsMarked = withTests 1 . property $ do
  -- The mark is drawn in whatever stands out against the stone it goes
  -- on, so it is drawn twice: once on Black and once on White.
  plain <- evalIO (drawn shown)
  black <- evalIO (drawn shown { gobanLast = Just (Coord 2 2) })
  white <- evalIO (drawn shown { gobanLast = Just (Coord 4 4) })
  assert (plain /= black)
  assert (plain /= white)
  assert (black /= white)

prop_aMarkOnAnEmptyPointDrawsNothing :: Property
prop_aMarkOnAnEmptyPointDrawsNothing = withTests 1 . property $ do
  -- The mark goes on the stone that was played last. A point with no
  -- stone on it has nothing to mark, which is what an undone game
  -- looks like for a moment.
  plain  <- evalIO (drawn shown)
  marked <- evalIO (drawn shown { gobanLast = Just (Coord 7 7) })
  plain === marked

prop_theCoordinatesCanBeLeftOff :: Property
prop_theCoordinatesCanBeLeftOff = withTests 1 . property $ do
  with'    <- evalIO (drawn shown)
  without  <- evalIO (drawn shown { gobanCoordinates = False })
  assert (with' /= without)

prop_theDarkBoardIsADifferentBoard :: Property
prop_theDarkBoardIsADifferentBoard = withTests 1 . property $ do
  light <- evalIO (pixels shown False Nothing 400 400)
  dark  <- evalIO (pixels shown True Nothing 400 400)
  assert (light /= dark)

prop_theFaintStoneFollowsThePointer :: Property
prop_theFaintStoneFollowsThePointer = withTests 1 . property $ do
  none' <- evalIO (pixels shown False Nothing 400 400)
  over  <- evalIO (pixels shown False (Just (Coord 6 6)) 400 400)
  assert (none' /= over)

prop_thereIsNoFaintStoneWhenItIsNotYourTurn :: Property
prop_thereIsNoFaintStoneWhenItIsNotYourTurn = withTests 1 . property $ do
  let waiting = shown { gobanHover = Nothing }
  none' <- evalIO (pixels waiting False Nothing 400 400)
  over  <- evalIO (pixels waiting False (Just (Coord 6 6)) 400 400)
  none' === over

prop_thereIsNoFaintStoneOnAPointThatHasOne :: Property
prop_thereIsNoFaintStoneOnAPointThatHasOne = withTests 1 . property $ do
  none' <- evalIO (pixels shown False Nothing 400 400)
  over  <- evalIO (pixels shown False (Just (Coord 2 2)) 400 400)
  none' === over

prop_aBoardDrawsAtAnySizeItIsGiven :: Property
prop_aBoardDrawsAtAnySizeItIsGiven = property $ do
  -- A window being dragged goes through every size on the way, and a
  -- board that throws at one of them takes the window with it.
  width  <- forAll (Gen.int (Range.linear 1 500))
  height <- forAll (Gen.int (Range.linear 1 500))
  _      <- evalIO (pixels shown False (Just (Coord 4 4)) width height)
  success

prop_anOblongWindowLeavesTheBoardSquare :: Property
prop_anOblongWindowLeavesTheBoardSquare = withTests 1 . property $ do
  -- The board is a square in the middle, so the far left of a wide
  -- window has nothing drawn on it.
  wide <- evalIO (pixels shown False Nothing 600 300)
  let row = ByteString.take 40 wide
  assert (blank row)
  assert (not (blank wide))

-- * What a click does
----------------------

-- | Where the middle of a point is, in a widget this size.
middleOf :: GobanProps -> Double -> Double -> Coord -> (Double, Double)
middleOf props width height =
  centreOf (geometry (size (gobanBoard props)) width height)

prop_aClickOnAnEmptyPointPlaysIt :: Property
prop_aClickOnAnEmptyPointPlaysIt = property $ do
  x <- forAll (Gen.int (Range.linear 0 8))
  y <- forAll (Gen.int (Range.linear 0 8))
  let wanted     = Coord x y
      (px, py)   = middleOf shown 400 400 wanted
      empty'     = shown { gobanBoard = emptyBoard 9 }
  clickAt empty' 400 400 px py === Just (GobanClicked wanted)

prop_aClickOnAStoneDoesNothing :: Property
prop_aClickOnAStoneDoesNothing = withTests 1 . property $ do
  let (px, py) = middleOf shown 400 400 (Coord 2 2)
  clickAt shown 400 400 px py === Nothing

prop_aClickWhenItIsNotYourTurnDoesNothing :: Property
prop_aClickWhenItIsNotYourTurnDoesNothing = withTests 1 . property $ do
  let waiting  = shown { gobanHover = Nothing }
      (px, py) = middleOf shown 400 400 (Coord 6 6)
  clickAt waiting 400 400 px py === Nothing

prop_aClickOffTheBoardDoesNothing :: Property
prop_aClickOffTheBoardDoesNothing = withTests 1 . property $ do
  clickAt shown 400 400 (-20) 200 === Nothing
  clickAt shown 400 400 200 (-20) === Nothing
  clickAt shown 400 400 900 200 === Nothing

prop_thePointerFindsOccupiedPointsToo :: Property
prop_thePointerFindsOccupiedPointsToo = withTests 1 . property $ do
  -- The pointer is over every point it is over. Leaving the faint
  -- stone off an occupied one is the drawing's business, and if this
  -- answered Nothing the faint stone would stay behind on the point
  -- before instead of going away.
  let (px, py) = middleOf shown 400 400 (Coord 2 2)
  pointerAt shown 400 400 px py === Just (Coord 2 2)
  pointerAt shown 400 400 (-20) 200 === Nothing

prop_aPointWithAStoneWouldNotTakeAnother :: Property
prop_aPointWithAStoneWouldNotTakeAnother = withTests 1 . property $ do
  wouldTakeAStone shown (Coord 2 2) === False
  wouldTakeAStone shown (Coord 4 4) === False
  wouldTakeAStone shown (Coord 6 6) === True
  wouldTakeAStone shown { gobanHover = Nothing } (Coord 6 6) === False

-- * What the handlers do
---------------------------

-- | The two references a board keeps: what it is showing, and where
-- the pointer is.
references
  :: GobanProps
  -> IO (IORef GobanProps, IORef (Maybe Coord))
references props = (,) <$> newIORef props <*> newIORef Nothing

prop_aClickIsPassedOnToWhoeverIsListening :: Property
prop_aClickIsPassedOnToWhoeverIsListening = withTests 1 . property $ do
  heard <- evalIO $ do
    (props, _) <- references shown
    listener   <- newIORef Nothing
    got        <- newIORef []
    writeIORef listener (Just (\event -> modifyIORef' got (event :)))
    let (px, py) = middleOf shown 400 400 (Coord 6 6)
    clicked props listener (400, 400) px py
    readIORef got
  heard === [GobanClicked (Coord 6 6)]

prop_aClickWithNobodyListeningIsDropped :: Property
prop_aClickWithNobodyListeningIsDropped = withTests 1 . property $ do
  -- Nobody is listening between one subscription being cancelled and
  -- the next one starting, which happens after every event.
  evalIO $ do
    (props, _) <- references shown
    listener   <- newIORef Nothing
    let (px, py) = middleOf shown 400 400 (Coord 6 6)
    clicked props listener (400, 400) px py
  success

prop_aClickOnNoPointTellsNobodyAnything :: Property
prop_aClickOnNoPointTellsNobodyAnything = withTests 1 . property $ do
  heard <- evalIO $ do
    (props, _) <- references shown
    listener   <- newIORef Nothing
    got        <- newIORef []
    writeIORef listener (Just (\event -> modifyIORef' got (event :)))
    clicked props listener (400, 400) (-30) 200
    let (px, py) = middleOf shown 400 400 (Coord 2 2)
    clicked props listener (400, 400) px py
    readIORef got
  heard === []

prop_theBoardIsRedrawnOnlyWhenThePointerChangesPoint :: Property
prop_theBoardIsRedrawnOnlyWhenThePointerChangesPoint =
  withTests 1 . property $ do
    -- The pointer moves many times between one point and the next, and
    -- a redraw for each would be the whole board drawn again to move
    -- one faint circle.
    (arrived, again, moved, gone, goneAgain) <- evalIO $ do
      (props, hover) <- references shown
      let (px, py) = middleOf shown 400 400 (Coord 6 6)
          (qx, qy) = middleOf shown 400 400 (Coord 7 7)
      arrived   <- pointerMoved props hover (400, 400) px py
      again     <- pointerMoved props hover (400, 400) (px + 1) (py + 1)
      moved     <- pointerMoved props hover (400, 400) qx qy
      gone      <- pointerLeft hover
      goneAgain <- pointerLeft hover
      pure (arrived, again, moved, gone, goneAgain)
    arrived === True
    again === False
    moved === True
    gone === True
    goneAgain === False

prop_thePointerRemembersWhereItIs :: Property
prop_thePointerRemembersWhereItIs = withTests 1 . property $ do
  where' <- evalIO $ do
    (props, hover) <- references shown
    let (px, py) = middleOf shown 400 400 (Coord 6 6)
    _ <- pointerMoved props hover (400, 400) px py
    readIORef hover
  where' === Just (Coord 6 6)

tests :: Group
tests = $$(discover)