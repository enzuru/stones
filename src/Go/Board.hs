-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

-- | The board and the rules that act on one stone at a time.
--
-- This module knows nothing about whose turn it is, about passing, or
-- about ko, which all need more than one position to decide. Those
-- live in "Go.Game".
module Go.Board
  ( Board
  , size
  , emptyBoard
  , stoneAt
  , inBounds
  , coords
  , neighbours
  , group
  , liberties
  , hasLiberty
  , Placement(..)
  , place
  , stoneCount
  )
where

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vector

import           Go.Types

-- | A square board of a given width, holding one value per point.
data Board = Board
  { size   :: !Int
    -- ^ The width of the board, which is 9, 13 or 19 in most games.
  , points :: !(Vector (Maybe Color))
  }
  deriving (Eq, Show)

-- | A board of this width with no stones on it.
emptyBoard :: Int -> Board
emptyBoard n = Board n (Vector.replicate (n * n) Nothing)

-- | The index into 'points' of a point that is on the board.
index :: Board -> Coord -> Int
index board (Coord x y) = y * size board + x

-- | Is this point on the board?
inBounds :: Board -> Coord -> Bool
inBounds board (Coord x y) =
  x >= 0 && y >= 0 && x < size board && y < size board

-- | The stone on this point, if there is one. A point off the board
-- is empty, which is what makes the liberty count below stop at the
-- edge.
stoneAt :: Board -> Coord -> Maybe Color
stoneAt board coord
  | inBounds board coord = points board Vector.! index board coord
  | otherwise            = Nothing

-- | Every point of the board, row by row from the top left.
coords :: Board -> [Coord]
coords board =
  [ Coord x y | y <- [0 .. size board - 1], x <- [0 .. size board - 1] ]

-- | The points beside this one, left out where the board ends.
neighbours :: Board -> Coord -> [Coord]
neighbours board (Coord x y) = filter
  (inBounds board)
  [Coord (x - 1) y, Coord (x + 1) y, Coord x (y - 1), Coord x (y + 1)]

-- | The stones joined to the one on this point, itself included. A
-- point with no stone on it has an empty group.
group :: Board -> Coord -> Set Coord
group board start = case stoneAt board start of
  Nothing    -> Set.empty
  Just color -> grow (Set.singleton start) [start]
   where
    grow seen [] = seen
    grow seen (current : rest) =
      let next =
            [ neighbour
            | neighbour <- neighbours board current
            , stoneAt board neighbour == Just color
            , not (Set.member neighbour seen)
            ]
      in  grow (foldl' (flip Set.insert) seen next) (next <> rest)

-- | The empty points beside a group.
liberties :: Board -> Set Coord -> Set Coord
liberties board stones = Set.fromList
  [ neighbour
  | stone     <- Set.toList stones
  , neighbour <- neighbours board stone
  , stoneAt board neighbour == Nothing
  ]

-- | Does the group on this point have anywhere left to breathe? This
-- answers without collecting the liberties, which is what the capture
-- check below wants.
hasLiberty :: Board -> Set Coord -> Bool
hasLiberty board stones = not (Set.null (liberties board stones))

-- | A move that was played, and what it did.
data Placement = Placement
  { placedBoard    :: !Board
    -- ^ The board after the stone went down and the dead came off.
  , placedCaptured :: !(Set Coord)
    -- ^ The points the captured stones came off.
  }
  deriving (Eq, Show)

-- | Put a stone down, take off whatever it kills, and refuse the move
-- if what is left has no liberties.
--
-- The order matters, and it is the order the rules give: the
-- opponent's dead stones come off before the new stone is asked
-- whether it can breathe. A stone that fills its own last liberty by
-- capturing is legal, and a stone that fills its own last liberty
-- without capturing is not.
place :: Color -> Coord -> Board -> Either Illegal Placement
place color coord board
  | not (inBounds board coord)    = Left OffBoard
  | stoneAt board coord /= Nothing = Left Occupied
  | not (hasLiberty cleared (group cleared coord)) = Left Suicide
  | otherwise = Right (Placement cleared captured)
 where
  placed = set (Just color) coord board

  -- The opponent groups beside the new stone that have just run out
  -- of liberties.
  captured = Set.unions
    [ dead
    | neighbour <- neighbours placed coord
    , stoneAt placed neighbour == Just (opposite color)
    , let dead = group placed neighbour
    , not (hasLiberty placed dead)
    ]

  cleared = foldl' (flip (set Nothing)) placed (Set.toList captured)

-- | Write a point, which is the one place the vector is touched.
set :: Maybe Color -> Coord -> Board -> Board
set value coord board =
  board { points = points board Vector.// [(index board coord, value)] }

-- | How many stones of this colour are on the board.
stoneCount :: Color -> Board -> Int
stoneCount color board =
  length (filter (== Just color) (Vector.toList (points board)))
