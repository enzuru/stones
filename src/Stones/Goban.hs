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
  { gobanBoard      :: !Board
    -- ^ The position to draw.
  , gobanLast       :: !(Maybe Coord)
    -- ^ The last stone played, which is marked.
  , gobanHover      :: !(Maybe Color)
    -- ^ The colour to show under the pointer, or 'Nothing' when it is
    -- not this player's turn and the board takes no clicks.
  , gobanCoordinates :: !Bool
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
  { currentProps :: IORef GobanProps
  , hoverPoint   :: IORef (Maybe Coord)
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
      shown <- readIORef props'
      point <- pointUnder area (boardWidth shown) x y
      heard <- readIORef sendTo
      case (point, heard) of
        (Just coord, Just callback) | playable shown coord ->
          callback (GobanClicked coord)
        _ -> pure ()
    Gtk.widgetAddController area click
    motion <- Gtk.eventControllerMotionNew
    _      <- Gtk.on motion #motion $ \x y -> do
      shown <- readIORef props'
      point <- pointUnder area (boardWidth shown) x y
      setHover area hover point
    _ <- Gtk.on motion #leave (setHover area hover Nothing)
    Gtk.widgetAddController area motion
    pure
      ( area
      , GobanState { currentProps = props'
                   , hoverPoint   = hover
                   , listener     = sendTo
                   }
      )

  customPatch old new state
    | old == new = CustomKeep
    | otherwise = CustomModify $ \area -> do
      writeIORef (currentProps state) new
      Gtk.widgetQueueDraw area
      pure state

  -- Subscribing is only saying where to send a click. The handlers are
  -- already connected, and an application resubscribes after every
  -- event it handles, so connecting them here would be work on every
  -- move for no change.
  customSubscribe _props GobanState { listener } _area callback = do
    writeIORef listener (Just callback)
    pure (fromCancellation (writeIORef listener Nothing))

-- | The width of the board being shown.
boardWidth :: GobanProps -> Int
boardWidth = size . gobanBoard

-- | Would a click here put a stone down? An occupied point and a board
-- that is not taking moves both answer no, and both are checked again
-- by the rules, which are what decides.
playable :: GobanProps -> Coord -> Bool
playable props coord =
  gobanHover props /= Nothing && stoneAt (gobanBoard props) coord == Nothing

-- | Which point a place in the widget is on, measured against the
-- widget's size now rather than the size it had when it was drawn.
pointUnder :: Gtk.DrawingArea -> Int -> Double -> Double -> IO (Maybe Coord)
pointUnder area n x y = do
  width  <- Gtk.widgetGetWidth area
  height <- Gtk.widgetGetHeight area
  pure (pointAt (geometry n (fromIntegral width) (fromIntegral height)) x y)

-- | Move the faint stone under the pointer, and redraw only when it
-- has actually moved to another point.
setHover :: Gtk.DrawingArea -> IORef (Maybe Coord) -> Maybe Coord -> IO ()
setHover area hovered point = do
  before <- readIORef hovered
  when (before /= point) $ do
    writeIORef hovered point
    Gtk.widgetQueueDraw area

-- * Drawing
------------

-- | The colours the board is drawn in.
data Palette = Palette
  { woodTop    :: (Double, Double, Double)
  , woodBottom :: (Double, Double, Double)
  , lineInk    :: (Double, Double, Double)
  , labelInk   :: (Double, Double, Double)
  }

-- | The wood is the same wood in both styles, a little darker in the
-- dark one, so that a lit board does not glare out of a dark window.
palette :: Bool -> Palette
palette dark
  | dark = Palette { woodTop    = (0.71, 0.54, 0.32)
                   , woodBottom = (0.60, 0.44, 0.25)
                   , lineInk    = (0.13, 0.10, 0.06)
                   , labelInk   = (0.25, 0.19, 0.12)
                   }
  | otherwise = Palette { woodTop    = (0.90, 0.74, 0.49)
                        , woodBottom = (0.83, 0.65, 0.39)
                        , lineInk    = (0.20, 0.15, 0.09)
                        , labelInk   = (0.33, 0.25, 0.15)
                        }

-- | Draw the whole board into a space this wide and this tall.
draw
  :: GobanProps -> Bool -> Maybe Coord -> Double -> Double -> Cairo.Render ()
draw props@GobanProps { gobanBoard, gobanLast, gobanCoordinates } dark hovered width height
  = do
    let n   = size gobanBoard
        geo = geometry n width height
        ink = palette dark
    drawWood ink geo
    drawGrid ink geo
    drawStars ink geo
    when gobanCoordinates (drawLabels ink geo)
    mapM_ (drawStoneAt geo) [ (c, s) | c <- coords gobanBoard
                            , Just s <- [stoneAt gobanBoard c] ]
    mapM_ (drawLastMark geo gobanBoard) gobanLast
    drawHover props geo hovered

-- | The wooden square, with a rounded edge and a line around it.
drawWood :: Palette -> Geometry -> Cairo.Render ()
drawWood Palette { woodTop, woodBottom } geo = do
  let radius = geoSide geo * 0.012
  roundedRectangle (geoLeft geo) (geoTop geo) (geoSide geo) (geoSide geo) radius
  Cairo.withLinearPattern (geoLeft geo)
                          (geoTop geo)
                          (geoLeft geo)
                          (geoTop geo + geoSide geo)
    $ \pattern' -> do
        addStop pattern' 0 woodTop
        addStop pattern' 1 woodBottom
        Cairo.setSource pattern'
        Cairo.fillPreserve
  Cairo.setSourceRGBA 0 0 0 0.22
  Cairo.setLineWidth (max 1 (geoSide geo * 0.002))
  Cairo.stroke
 where
  addStop pattern' at (r, g, b) = Cairo.patternAddColorStopRGB pattern' at r g b

-- | The lines of the grid. The four at the edge are drawn thicker,
-- which is how a board is painted.
drawGrid :: Palette -> Geometry -> Cairo.Render ()
drawGrid Palette { lineInk = (r, g, b) } geo = do
  Cairo.setSourceRGB r g b
  Cairo.setLineCap Cairo.LineCapSquare
  mapM_ column [0 .. n - 1]
  mapM_ row    [0 .. n - 1]
 where
  n    = geoSize geo
  thin = max 0.7 (geoStep geo * 0.035)

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
drawStars Palette { lineInk = (r, g, b) } geo = do
  Cairo.setSourceRGB r g b
  mapM_ dot (starPoints (geoSize geo))
 where
  dot coord = do
    let (x, y) = centreOf geo coord
    Cairo.arc x y (max 1.2 (geoStep geo * 0.09)) 0 (2 * pi)
    Cairo.fill

-- | The column letters along the bottom and the row numbers down the
-- left, which is where a player reading a game record looks.
drawLabels :: Palette -> Geometry -> Cairo.Render ()
drawLabels Palette { labelInk = (r, g, b) } geo = do
  let n = geoSize geo
  Cairo.setSourceRGB r g b
  Cairo.selectFontFace ("Cantarell" :: Text)
                       Cairo.FontSlantNormal
                       Cairo.FontWeightNormal
  Cairo.setFontSize (geoMargin geo * 0.72)
  mapM_ (column n) [0 .. n - 1]
  mapM_ (row n)    [0 .. n - 1]
 where
  column n i = centred (columnLabelAt geo i) (Text.singleton (columnLetters n !! i))
  row n i = centred (rowLabelAt geo i) (Text.pack (show (n - i)))

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
      radius = geoStone geo
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
        radius = geoStone geo
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
drawHover props geo hovered = case (gobanHover props, hovered) of
  (Just color, Just coord) | playable props coord -> do
    let (x, y)       = centreOf geo coord
        radius       = geoStone geo
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
