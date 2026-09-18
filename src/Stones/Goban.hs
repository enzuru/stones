-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The board, as a widget that draws itself and reports where it was
-- clicked.
--
-- The board is a 'Gtk.DrawingArea' rather than a grid of widgets. A
-- 19x19 board is 361 points, and the lines between them are what the
-- eye reads the shape by, so drawing the whole thing at once is both
-- faster and closer to what a board looks like.
--
-- Where the pointer is hovering is held here rather than in the
-- application's state. The pointer moves many times a second, and an
-- application that took each move as an event would rebuild its whole
-- window that many times to move one faint circle. Nothing outside
-- this widget needs to know where the pointer is, so nothing outside
-- it is told.
module Stones.Goban
  ( GobanProps(..)
  , GobanEvent(..)
  , goban
    -- * What the widget decides, without the widget
    --
    -- $inner
  , clickAt
  , pointerAt
  , wouldTakeAStone
  , clicked
  , pointerMoved
  , pointerLeft
  , draw
  )
where

import           Control.Monad                  ( when )
import           Data.IORef
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Vector                    ( Vector )

import qualified GI.Cairo.Render               as Cairo
import           GI.Cairo.Render.Connector      ( renderWithContext )
import qualified GI.Adw                        as Adw
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource ( fromCancellation )

import           Go.Board
import           Go.Types
import           Go.Vertex                      ( columnLetters )
import           Stones.Goban.Geometry

-- | What the board shows.
data GobanProps = GobanProps
  { board      :: !Board
    -- ^ The position to draw.
  , last       :: !(Maybe Coord)
    -- ^ The last stone played, which is marked.
  , hover      :: !(Maybe Color)
    -- ^ The colour to show under the pointer, or 'Nothing' when it is
    -- not this player's turn and the board takes no clicks.
  , coordinates :: !Bool
    -- ^ Whether to write the column letters and row numbers.
  }
  deriving (Eq, Show)

-- | What the board reports.
newtype GobanEvent = GobanClicked Coord
  deriving (Eq, Show)

-- | What the widget keeps between patches.
--
-- The controllers are not kept. A widget takes a controller over when
-- it is given one, and reading the value afterwards is reading
-- something the Haskell side no longer owns, which the bindings warn
-- about. So the handlers are connected once, when the widget is made,
-- and what changes between subscriptions is the box they send to.
data GobanState = GobanState
  { props :: IORef GobanProps
  , hover   :: IORef (Maybe Coord)
  , listener     :: IORef (Maybe (GobanEvent -> IO ()))
    -- ^ Where a click goes, when anybody is listening.
  }

-- | A board.
goban
  :: Vector (Attribute Gtk.DrawingArea GobanEvent)
  -> GobanProps
  -> Widget GobanEvent
goban customAttributes customParams = Widget CustomWidget { .. }
 where
  customWidget = Gtk.DrawingArea

  customCreate props = do
    area  <- Gtk.new Gtk.DrawingArea [#hexpand Gtk.:= True, #vexpand Gtk.:= True]
    props' <- newIORef props
    hover  <- newIORef Nothing
    -- The draw function reads both references rather than closing over
    -- the values, so that a patch and a pointer move are a write and a
    -- redraw rather than a new draw function each time.
    Gtk.drawingAreaSetDrawFunc area . Just $ \_area context width height -> do
      shown   <- readIORef props'
      hovered <- readIORef hover
      -- The style manager is asked here rather than passed in, because
      -- GTK redraws the widget when the theme changes and does not
      -- tell the application, so this is the one place that hears
      -- about it at the right moment.
      dark    <- Adw.styleManagerGetDefault >>= Adw.styleManagerGetDark
      _       <- renderWithContext
        (draw shown dark hovered (fromIntegral width) (fromIntegral height))
        context
      pure ()
    -- The controllers are added once, here, rather than in
    -- 'customSubscribe', which runs again after every event the
    -- application handles. Adding them there would stack a new pair on
    -- the widget every time.
    sendTo <- newIORef Nothing
    click  <- Gtk.gestureClickNew
    _      <- Gtk.on click #pressed $ \_presses x y -> do
      size' <- sizeOf area
      clicked props' sendTo size' x y
    Gtk.widgetAddController area click
    motion <- Gtk.eventControllerMotionNew
    _      <- Gtk.on motion #motion $ \x y -> do
      size' <- sizeOf area
      moved <- pointerMoved props' hover size' x y
      when moved (Gtk.widgetQueueDraw area)
    _ <- Gtk.on motion #leave $ do
      moved <- pointerLeft hover
      when moved (Gtk.widgetQueueDraw area)
    Gtk.widgetAddController area motion
    pure
      ( area
      , GobanState { props = props', hover = hover, listener = sendTo }
      )

  -- The state is named in the pattern as well as bound, because a
  -- field read through a record dot leaves the type of what it was
  -- read from to be worked out, and this one is an argument of a
  -- record whose type says nothing about it.
  customPatch old new state@GobanState { props }
    | old == new = CustomKeep
    | otherwise = CustomModify $ \area -> do
      writeIORef props new
      Gtk.widgetQueueDraw area
      pure state

  -- Subscribing is only saying where to send a click. The handlers are
  -- already connected, and an application resubscribes after every
  -- event it handles, so connecting them here would be work on every
  -- move for no change.
  customSubscribe _props GobanState { listener } _area callback = do
    writeIORef listener (Just callback)
    pure (fromCancellation (writeIORef listener Nothing))

-- $inner
--
-- What the widget does with a click, with a pointer, and with a blank
-- surface, as functions of their arguments rather than of a widget
-- that has to be on a screen. The handlers above are these with the
-- widget's size read out of the widget, and the tests are these with a
-- size written down.

-- | The width of the board being shown.
boardWidth :: GobanProps -> Int
boardWidth props = props.board.size

-- | The board as it is laid out in a widget this wide and this tall.
layout :: GobanProps -> Double -> Double -> Geometry
layout props = geometry (boardWidth props)

-- | Would a click here put a stone down? An occupied point and a board
-- that is not taking moves both answer no, and both are checked again
-- by the rules, which are what decides.
wouldTakeAStone :: GobanProps -> Coord -> Bool
wouldTakeAStone props coord =
  props.hover /= Nothing && stoneAt props.board coord == Nothing

-- | What a click at this place in a widget this size does, which is
-- nothing at all unless it lands on a point that would take a stone.
clickAt
  :: GobanProps -> Double -> Double -> Double -> Double -> Maybe GobanEvent
clickAt props width height x y = do
  coord <- pointAt (layout props width height) x y
  if wouldTakeAStone props coord then Just (GobanClicked coord) else Nothing

-- | The point the pointer is over, which is where the faint stone
-- goes. Every point counts, whether or not a stone could go on it:
-- the drawing is what leaves the faint stone off an occupied point,
-- so that the pointer moving onto one takes the stone away rather
-- than leaving it behind on the point before.
pointerAt
  :: GobanProps -> Double -> Double -> Double -> Double -> Maybe Coord
pointerAt props width height = pointAt (layout props width height)

-- | What a click on the board does: nothing, or tell whoever is
-- listening which point it landed on.
--
-- Nobody is listening between a subscription being cancelled and the
-- next one starting, which is a moment that happens after every event
-- the application handles, so a click has somewhere to go or it has
-- nowhere to go and is dropped.
clicked
  :: IORef GobanProps
  -> IORef (Maybe (GobanEvent -> IO ()))
  -> (Double, Double)
    -- ^ How big the board is.
  -> Double
  -> Double
    -- ^ Where the click was.
  -> IO ()
clicked props listener (width, height) x y = do
  shown <- readIORef props
  heard <- readIORef listener
  case (heard, clickAt shown width height x y) of
    (Just callback, Just event) -> callback event
    _                           -> pure ()

-- | The pointer is over this place on a board this size. The answer is
-- whether the board has to be drawn again, which it does only when the
-- pointer has moved from one point to another.
pointerMoved
  :: IORef GobanProps
  -> IORef (Maybe Coord)
  -> (Double, Double)
  -> Double
  -> Double
  -> IO Bool
pointerMoved props hovered (width, height) x y = do
  shown <- readIORef props
  setHover hovered (pointerAt shown width height x y)

-- | The pointer has gone, so the faint stone goes with it.
pointerLeft :: IORef (Maybe Coord) -> IO Bool
pointerLeft hovered = setHover hovered Nothing

-- | Write down where the pointer is, and say whether that is news.
setHover :: IORef (Maybe Coord) -> Maybe Coord -> IO Bool
setHover hovered point = do
  before <- readIORef hovered
  writeIORef hovered point
  pure (before /= point)

-- | How big the widget is now, rather than how big it was when it was
-- last drawn.
sizeOf :: Gtk.DrawingArea -> IO (Double, Double)
sizeOf area = do
  width  <- Gtk.widgetGetWidth area
  height <- Gtk.widgetGetHeight area
  pure (fromIntegral width, fromIntegral height)

-- * Drawing
------------

-- | The colours the board is drawn in.
data Palette = Palette
  { top    :: (Double, Double, Double)
  , bottom :: (Double, Double, Double)
  , line    :: (Double, Double, Double)
  , label   :: (Double, Double, Double)
  }

-- | The wood is the same wood in both styles, a little darker in the
-- dark one, so that a lit board does not glare out of a dark window.
palette :: Bool -> Palette
palette dark
  | dark = Palette { top    = (0.71, 0.54, 0.32)
                   , bottom = (0.60, 0.44, 0.25)
                   , line    = (0.13, 0.10, 0.06)
                   , label   = (0.25, 0.19, 0.12)
                   }
  | otherwise = Palette { top    = (0.90, 0.74, 0.49)
                        , bottom = (0.83, 0.65, 0.39)
                        , line    = (0.20, 0.15, 0.09)
                        , label   = (0.33, 0.25, 0.15)
                        }

-- | Draw the whole board into a space this wide and this tall.
draw
  :: GobanProps -> Bool -> Maybe Coord -> Double -> Double -> Cairo.Render ()
draw props@GobanProps { board, last = played, coordinates } dark hovered width height
  = do
    let n   = board.size
        geo = geometry n width height
        ink = palette dark
    drawWood ink geo
    drawGrid ink geo
    drawStars ink geo
    when coordinates (drawLabels ink geo)
    mapM_ (drawStoneAt geo)
          [ (point, stone)
          | point        <- coords board
          , Just stone   <- [stoneAt board point]
          ]
    mapM_ (drawLastMark geo board) played
    drawHover props geo hovered

-- | The wooden square, with a rounded edge and a line around it.
drawWood :: Palette -> Geometry -> Cairo.Render ()
drawWood Palette { top, bottom } geo = do
  let radius = geo.side * 0.012
  roundedRectangle geo.left geo.top geo.side geo.side radius
  Cairo.withLinearPattern geo.left
                          geo.top
                          geo.left
                          (geo.top + geo.side)
    $ \pattern' -> do
        addStop pattern' 0 top
        addStop pattern' 1 bottom
        Cairo.setSource pattern'
        Cairo.fillPreserve
  Cairo.setSourceRGBA 0 0 0 0.22
  Cairo.setLineWidth (max 1 (geo.side * 0.002))
  Cairo.stroke
 where
  addStop pattern' at (r, g, b) = Cairo.patternAddColorStopRGB pattern' at r g b

-- | The lines of the grid. The four at the edge are drawn thicker,
-- which is how a board is painted.
drawGrid :: Palette -> Geometry -> Cairo.Render ()
drawGrid Palette { line = (r, g, b) } geo = do
  Cairo.setSourceRGB r g b
  Cairo.setLineCap Cairo.LineCapSquare
  mapM_ column [0 .. n - 1]
  mapM_ row    [0 .. n - 1]
 where
  n    = geo.size
  thin = max 0.7 (geo.step * 0.035)

  column i = segment (widthOf i) (centreOf geo (Coord i 0))
                                 (centreOf geo (Coord i (n - 1)))
  row i = segment (widthOf i) (centreOf geo (Coord 0 i))
                              (centreOf geo (Coord (n - 1) i))

  -- The four lines at the edge of the grid are drawn thicker, which is
  -- how a board is painted.
  widthOf i | i == 0 || i == n - 1 = thin * 2
            | otherwise            = thin

  segment lineWidth (x0, y0) (x1, y1) = do
    Cairo.setLineWidth lineWidth
    Cairo.moveTo x0 y0
    Cairo.lineTo x1 y1
    Cairo.stroke

-- | The marked points.
drawStars :: Palette -> Geometry -> Cairo.Render ()
drawStars Palette { line = (r, g, b) } geo = do
  Cairo.setSourceRGB r g b
  mapM_ dot (starPoints geo.size)
 where
  dot coord = do
    let (x, y) = centreOf geo coord
    Cairo.arc x y (max 1.2 (geo.step * 0.09)) 0 (2 * pi)
    Cairo.fill

-- | The column letters along the bottom and the row numbers down the
-- left, which is where a player reading a game record looks.
drawLabels :: Palette -> Geometry -> Cairo.Render ()
drawLabels Palette { label = (r, g, b) } geo = do
  Cairo.setSourceRGB r g b
  Cairo.selectFontFace ("Cantarell" :: Text)
                       Cairo.FontSlantNormal
                       Cairo.FontWeightNormal
  Cairo.setFontSize (geo.margin * 0.72)
  -- Over the letters themselves rather than over the numbers of the
  -- columns, so that there is no counting to get wrong.
  mapM_ column (zip [0 ..] (columnLetters n))
  mapM_ row    [0 .. n - 1]
 where
  n = geo.size
  column (i, letter) = centred (columnLabelAt geo i) (Text.singleton letter)
  row i = centred (rowLabelAt geo i) (Text.pack (show (n - i)))

  -- Cairo puts text where the baseline starts, and a label wants to be
  -- middled on the line it belongs to, so the extents say how far to
  -- move back.
  centred (x, y) text = do
    extents <- Cairo.textExtents text
    Cairo.moveTo
      (x - Cairo.textExtentsWidth extents / 2 - Cairo.textExtentsXbearing extents)
      (y - Cairo.textExtentsHeight extents / 2 - Cairo.textExtentsYbearing extents)
    Cairo.showText text

-- | One stone, with a shadow under it and a highlight on it, so that
-- black on a dark line and white on light wood both stand out.
drawStoneAt :: Geometry -> (Coord, Color) -> Cairo.Render ()
drawStoneAt geo (coord, color) = do
  let (x, y) = centreOf geo coord
      radius = geo.stone
  Cairo.setSourceRGBA 0 0 0 0.28
  Cairo.arc (x + radius * 0.07) (y + radius * 0.09) radius 0 (2 * pi)
  Cairo.fill
  Cairo.withRadialPattern (x - radius * 0.35)
                          (y - radius * 0.4)
                          (radius * 0.08)
                          x
                          y
                          radius
    $ \pattern' -> do
        let (near, far) = stoneShades color
        stop pattern' 0 near
        stop pattern' 1 far
        Cairo.setSource pattern'
        Cairo.arc x y radius 0 (2 * pi)
        Cairo.fill
  when (color == White) $ do
    Cairo.setSourceRGBA 0 0 0 0.20
    Cairo.setLineWidth (max 0.6 (radius * 0.05))
    Cairo.arc x y (radius * 0.97) 0 (2 * pi)
    Cairo.stroke
 where
  stop pattern' at (r, g, b) = Cairo.patternAddColorStopRGB pattern' at r g b

-- | The lit side and the far side of a stone.
stoneShades
  :: Color -> ((Double, Double, Double), (Double, Double, Double))
stoneShades Black = ((0.42, 0.42, 0.46), (0.04, 0.04, 0.07))
stoneShades White = ((1.00, 1.00, 0.99), (0.79, 0.78, 0.75))

-- | The ring on the stone that was played last.
drawLastMark :: Geometry -> Board -> Coord -> Cairo.Render ()
drawLastMark geo board coord = case stoneAt board coord of
  Nothing    -> pure ()
  Just color -> do
    let (x, y) = centreOf geo coord
        radius = geo.stone
    case color of
      Black -> Cairo.setSourceRGBA 1 1 1 0.85
      White -> Cairo.setSourceRGBA 0 0 0 0.7
    Cairo.setLineWidth (max 1 (radius * 0.16))
    Cairo.arc x y (radius * 0.4) 0 (2 * pi)
    Cairo.stroke

-- | The faint stone under the pointer, which says where a click would
-- put one. It is not drawn when the board is not taking moves, or when
-- the point already has a stone on it.
drawHover :: GobanProps -> Geometry -> Maybe Coord -> Cairo.Render ()
drawHover props geo hovered = case (props.hover, hovered) of
  (Just color, Just coord) | wouldTakeAStone props coord -> do
    let (x, y)       = centreOf geo coord
        radius       = geo.stone
        (_, (r, g, b)) = stoneShades color
    Cairo.setSourceRGBA r g b 0.45
    Cairo.arc x y radius 0 (2 * pi)
    Cairo.fill
  _ -> pure ()

-- | A rectangle with rounded corners, which cairo has no call of its
-- own for.
roundedRectangle
  :: Double -> Double -> Double -> Double -> Double -> Cairo.Render ()
roundedRectangle x y width height radius = do
  Cairo.newPath
  Cairo.arc (x + width - radius) (y + radius) radius (-pi / 2) 0
  Cairo.arc (x + width - radius) (y + height - radius) radius 0 (pi / 2)
  Cairo.arc (x + radius)         (y + height - radius) radius (pi / 2) pi
  Cairo.arc (x + radius)         (y + radius) radius pi (3 * pi / 2)
  Cairo.closePath
