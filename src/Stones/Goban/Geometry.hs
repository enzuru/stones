-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

-- | Where the lines and the stones of a board go, in the space a
-- widget has been given.
--
-- This is separate from the drawing so that the two things that need
-- it agree. The drawing turns a point into a place on the screen, and
-- a click turns a place on the screen back into a point, and a board
-- where the stones land beside the pointer is a board where those two
-- have drifted apart.
module Stones.Goban.Geometry
  ( Geometry(..)
  , geometry
  , centreOf
  , pointAt
  , starPoints
  , columnLabelAt
  , rowLabelAt
  )
where

import           Go.Types

-- | The board drawn in a given space.
data Geometry = Geometry
  { geoSize   :: !Int
    -- ^ The width of the board in points.
  , geoLeft   :: !Double
  , geoTop    :: !Double
    -- ^ The top left corner of the wooden square.
  , geoSide   :: !Double
    -- ^ The width of the wooden square.
  , geoMargin :: !Double
    -- ^ The wood between the edge of the square and the outermost
    -- line, which is where the coordinate labels are written.
  , geoStep   :: !Double
    -- ^ The distance between one line and the next.
  , geoStone  :: !Double
    -- ^ The radius of a stone.
  }
  deriving (Eq, Show)

-- | Fit a board of this width into a space this wide and this tall.
--
-- The board is a square in the middle of the space, because a board
-- drawn to fill an oblong is a board whose stones are eggs.
geometry :: Int -> Double -> Double -> Geometry
geometry n width height = Geometry { geoSize   = n
                                   , geoLeft   = (width - side) / 2
                                   , geoTop    = (height - side) / 2
                                   , geoSide   = side
                                   , geoMargin = margin
                                   , geoStep   = step
                                   , geoStone  = step * 0.47
                                   }
 where
  side = max 1 (min width height)
  -- The margin holds the coordinate labels. It grows with the spacing
  -- of the lines, so that a 9x9 board is not drawn with the hairline
  -- margin a 19x19 board of the same width wants, and it is capped, so
  -- that a small board does not end up mostly margin.
  margin = min (side * 0.06) (side / fromIntegral (n + 2))
  step | n <= 1    = 0
       | otherwise = (side - 2 * margin) / fromIntegral (n - 1)

-- | Where the middle of a point is.
centreOf :: Geometry -> Coord -> (Double, Double)
centreOf geo (Coord x y) =
  ( geoLeft geo + geoMargin geo + fromIntegral x * geoStep geo
  , geoTop geo + geoMargin geo + fromIntegral y * geoStep geo
  )

-- | The point a click at this place landed on, if it landed on one.
--
-- A click counts for the nearest point when it is within half a step
-- of it, which is the whole board and nothing outside it.
pointAt :: Geometry -> Double -> Double -> Maybe Coord
pointAt geo x y
  | geoStep geo <= 0                  = Nothing
  | column < 0 || column >= geoSize geo = Nothing
  | row < 0 || row >= geoSize geo      = Nothing
  | otherwise                          = Just (Coord column row)
 where
  column = nearest (x - geoLeft geo - geoMargin geo)
  row    = nearest (y - geoTop geo - geoMargin geo)
  nearest distance = round (distance / geoStep geo)

-- | The marked points of a board of this width.
--
-- A 19x19 board has nine, on the third rows and the middle. The
-- smaller boards have five, and a board smaller than 7x7 has none,
-- because there is nowhere to put them that is not already an edge.
starPoints :: Int -> [Coord]
starPoints n
  | n < 7            = []
  | n >= 19 && odd n = [ Coord x y | x <- lines', y <- lines' ]
  | odd n            = corners <> [Coord mid mid]
  | otherwise        = corners
 where
  edge    = if n >= 13 then 3 else 2
  mid     = (n - 1) `div` 2
  lines'  = [edge, mid, n - 1 - edge]
  corners = [ Coord x y | x <- [edge, n - 1 - edge], y <- [edge, n - 1 - edge] ]

-- | Where a column label goes: under the bottom of that column, in the
-- margin. The answer is the middle of the text, which the drawing
-- shifts by half the width of the label.
columnLabelAt :: Geometry -> Int -> (Double, Double)
columnLabelAt geo column =
  let (x, _) = centreOf geo (Coord column 0)
  in  (x, geoTop geo + geoSide geo - geoMargin geo * 0.5)

-- | Where a row label goes: left of that row, in the margin.
rowLabelAt :: Geometry -> Int -> (Double, Double)
rowLabelAt geo row =
  let (_, y) = centreOf geo (Coord 0 row)
  in  (geoLeft geo + geoMargin geo * 0.5, y)
