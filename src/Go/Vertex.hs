-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}

-- | The names the Go Text Protocol gives to points on the board.
--
-- A vertex is a column letter and a row number, @D4@ or @Q16@. The
-- letter @I@ is left out of the columns, because it reads as a one on
-- a board drawn in text, so the nineteenth column is @T@. The rows are
-- counted from the bottom, and 'Coord' counts them from the top, so
-- every conversion here turns the row over.
module Go.Vertex
  ( columnLetters
  , columnLetter
  , toVertex
  , fromVertex
  , moveToVertex
  , moveFromVertex
  )
where

import           Data.Char                      ( isDigit
                                                , toUpper
                                                )
import           Data.List                      ( elemIndex )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Text.Read                      ( readMaybe )

import           Go.Types

-- | The column letters, in order, for a board of this width. @I@ is
-- not one of them.
columnLetters :: Int -> [Char]
columnLetters n = take n (filter (/= 'I') ['A' .. 'Z'])

-- | The letter of one column of a board of this width, if there is a
-- column that far along.
columnLetter :: Int -> Int -> Maybe Char
columnLetter n x
  | x < 0     = Nothing
  | otherwise = case drop x (columnLetters n) of
    letter : _ -> Just letter
    []         -> Nothing

-- | The name of a point on a board of this width, or nothing when it
-- is not a point on a board that wide.
--
-- The answer is a 'Maybe' for the same reason 'fromVertex' gives one:
-- a name and a point on a board of a given width are two ways of
-- saying the same thing, and either way round there are values on one
-- side that have nothing on the other.
toVertex :: Int -> Coord -> Maybe Text
toVertex n (Coord x y)
  | y < 0 || y >= n = Nothing
  | otherwise = do
    letter <- columnLetter n x
    pure (Text.pack (letter : show (n - y)))

-- | The point a name stands for, on a board of this width. A name
-- that is not a point on this board, including @pass@, gives
-- 'Nothing'.
fromVertex :: Int -> Text -> Maybe Coord
fromVertex n text = case Text.unpack (Text.strip text) of
  letter : rest | all isDigit rest && not (null rest) -> do
    x   <- elemIndex (toUpper letter) (columnLetters n)
    row <- readMaybe rest
    if row >= 1 && row <= n then Just (Coord x (n - row)) else Nothing
  _ -> Nothing

-- | What the protocol calls a move, or nothing when the move is on a
-- point that is not on a board this wide.
--
-- A resignation has no name of its own in a @play@ command, so it is
-- sent as a pass. A program that resigns says so by other means.
moveToVertex :: Int -> Move -> Maybe Text
moveToVertex n move = case move of
  Play coord -> toVertex n coord
  Pass       -> Just "pass"
  Resign     -> Just "pass"

-- | The move a protocol answer stands for. The protocol is not
-- case-sensitive, and GNU Go answers in capitals.
moveFromVertex :: Int -> Text -> Maybe Move
moveFromVertex n text = case Text.toLower (Text.strip text) of
  "pass"   -> Just Pass
  "resign" -> Just Resign
  _        -> Play <$> fromVertex n text
