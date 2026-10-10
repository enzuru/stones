-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE NoFieldSelectors      #-}

-- | What a game is started from, and the page that asks for it.
--
-- A tab opens on this page rather than on a board. The program has
-- nothing to go on when it starts: which board, which colour, and how
-- hard the opponent should try are three things only the player knows,
-- and starting a game before asking is guessing at all three.
--
-- Nothing is started while this page is up. The opponent is a process,
-- and it is not worth one until somebody has asked for a game.
module Stones.Setup
  ( Setup(..)
  , Against(..)
  , places
  , describePlace
  , Speed(..)
  , speeds
  , describeSpeed
  , speedName
  , Ongoing(..)
  , Signing(..)
  , describeSigning
  , signedIn
  , versusOf
  , Strength(..)
  , strengths
  , describeStrength
  , strengthNumber
  , handicaps
  , describeHandicap
  , Handicapping(..)
  , handicappings
  , describeHandicapping
  , SetupEvent(..)
  , defaultSetup
  , boardWidths
  , chose
  , describeSetup
  , launchPage
  )
where

import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           Data.Text                      ( Text )
import qualified Data.Vector                   as Vector
import qualified Data.Text                     as Text

import qualified GI.Adw                        as Adw
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Adwaita.Bin ( )
import           GI.Gtk.Declarative.Adwaita.ComboRow
                                                ( ComboRowParams(..)
                                                , choice
                                                , comboRow
                                                , defaultComboRowParams
                                                )
import           GI.Gtk.Declarative.Adwaita.Rows
                                                ( headerSuffix
                                                , rowSuffix
                                                )
import           GI.Gtk.Declarative.Adwaita.ToggleGroup
                                                ( ToggleGroupParams(..)
                                                , defaultToggleGroupParams
                                                , toggle
                                                , toggleGroup
                                                )

import           Go.Types

-- | How hard an opponent is asked to try.
--
-- One to ten, which is the range GNU Go itself takes. It used to be
-- three words, on the grounds that ten steps is more than anybody
-- wants to read. Ten numbers say the same thing without pretending
-- there are only three of them, and somebody who wants the one
-- between fair and fierce can have it.
newtype Strength = Strength Int
  deriving (Eq, Ord, Show)

-- | Every strength, easiest first, which is the order a menu shows
-- them in.
strengths :: [Strength]
strengths = map Strength [1 .. 10]

-- | What to call one in the window.
describeStrength :: Strength -> Text
describeStrength (Strength n) = Text.pack (show n)

-- | The number in one, for whoever has to hand it to an engine.
strengthNumber :: Strength -> Int
strengthNumber (Strength n) = n

-- | The strength that goes by this name, or the hardest one, since a
-- name no strength answers to is not a name this page ever wrote.
strengthNamed :: Text -> Strength
strengthNamed name =
  fromMaybe (Strength 10) (listToMaybe [ w | w <- strengths, describeStrength w == name ])

-- | Where a game is played.
--
-- Two places, and they ask for different things. A program on this
-- machine is told which colour to take and how hard to try. A game
-- somebody else is paired into settles both of those on the other
-- side, and asks instead who the player is.
data Against
  = Computer
    -- ^ Against a program on this machine.
  | Online
    -- ^ Against whoever online-go.com pairs the player with.
  deriving (Eq, Show, Enum, Bounded)

-- | Both, in the order the page offers them.
places :: [Against]
places = [minBound .. maxBound]

-- | What to call one in the window.
describePlace :: Against -> Text
describePlace Computer = "This computer"
describePlace Online   = "online-go.com"

-- | What to ask a server about handicaps.
--
-- Not a number. Nobody picks how many stones a game somebody else is
-- paired into starts with: the ranks decide that, and the server says
-- where the game starts when it is joined, stones and all. What there
-- is to ask for is whether to be paired into one at all.
data Handicapping
  = Required
    -- ^ Only a game with a handicap on it.
  | Standard
    -- ^ Whatever the two ranks call for, which is what most people
    -- want and what the server does when nobody says otherwise.
  | Disabled
    -- ^ An even board, whoever is on the other side of it.
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Every one of them, in the order the page offers them.
handicappings :: [Handicapping]
handicappings = [minBound .. maxBound]

-- | What to call one in the window.
describeHandicapping :: Handicapping -> Text
describeHandicapping = \case
  Required -> "Required"
  Standard -> "Standard"
  Disabled -> "Disabled"

-- | How long a game is allowed to take.
--
-- Only a game on a server asks. A program on this machine answers as
-- fast as it answers, and nobody is waiting on the other side of it.
data Speed
  = Blitz
  | Rapid
  | Live
  | Correspondence
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Every speed, quickest first, which is the order a menu shows them.
speeds :: [Speed]
speeds = [minBound .. maxBound]

-- | What to call one in the window.
describeSpeed :: Speed -> Text
describeSpeed = \case
  Blitz          -> "Blitz"
  Rapid          -> "Rapid"
  Live           -> "Live"
  Correspondence -> "Daily"

-- | What the server calls one.
speedName :: Speed -> Text
speedName = \case
  Blitz          -> "blitz"
  Rapid          -> "rapid"
  Live           -> "live"
  Correspondence -> "correspondence"

-- | A game this player is already in, that this window has no tab for.
--
-- Started on the website, or in a tab that was closed, or days ago and
-- waiting for a move ever since. Nothing has to be agreed to open one:
-- it is already going, and this is only a way back into it.
data Ongoing = Ongoing
  { with  :: Text
    -- ^ Who it is against.
  , about :: Setup
    -- ^ What it is, as far as it can be said in the terms the rest of
    -- this program uses.
  , game  :: Int
    -- ^ The number the server knows it by.
  }
  deriving (Eq, Show)

-- | How far the player has got with signing in.
--
-- Only one place asks for it, and the page only shows this when that
-- is the place chosen.
data Signing
  = SignedOut
  | SigningIn
    -- ^ The browser is open and the server has not answered yet.
  | SignedIn Text
    -- ^ Signed in, and this is what to call the player.
  | SignInRefused Text
    -- ^ It did not work, and this is why.
  deriving (Eq, Show)

-- | The line the sign-in row shows.
describeSigning :: Signing -> Text
describeSigning = \case
  SignedOut         -> "Not signed in"
  SigningIn         -> "Waiting for the browser\8230"
  SignedIn name     -> "Signed in as " <> name
  SignInRefused why -> why

-- | Is the player signed in?
signedIn :: Signing -> Bool
signedIn (SignedIn _) = True
signedIn _            = False

-- | What a game will be started with.
data Setup = Setup
  { against  :: Against
    -- ^ Where the game is played, which decides what else is asked.
  , size     :: Int
    -- ^ The width of the board.
  , human    :: Color
    -- ^ The colour the player takes.
  , strength :: Strength
    -- ^ How hard the opponent is asked to try.
  , handicap :: Int
    -- ^ How many stones the player starts with down, or none.
  , speed    :: Speed
    -- ^ How long a game on a server is allowed to take.
  , handicapping :: Handicapping
    -- ^ What to ask a server about handicaps, which is a preference
    -- rather than the number the other place takes.
  , ranked   :: Bool
    -- ^ Whether a game on a server counts towards a rank.
  , versus   :: Text
    -- ^ Who to ask for a game, by name, or nobody in particular.
    --
    -- Only a game on a server has one. Empty means whoever else is
    -- looking for the same game, which is what most people want.
    -- Naming somebody is how a program is played against on purpose,
    -- which is what this program is tried out against.
  }
  deriving (Eq, Show)

-- | The handicaps on offer.
--
-- Two is the smallest that means anything, and five is as many as the
-- smallest board takes, so these are the ones that work whatever board
-- is picked. The command line goes as far as the engine will.
handicaps :: [Int]
handicaps = [0, 2, 3, 4, 5]

-- | What to call one in the window.
describeHandicap :: Int -> Text
describeHandicap 0 = "None"
describeHandicap n = Text.pack (show n)

-- | The name to ask for a game by, with the spaces either side taken
-- off.
--
-- Taken off here rather than as it is typed. The page shows what it
-- holds, so trimming what was typed would take a space away from under
-- somebody's cursor while they were still typing the next word.
versusOf :: Setup -> Text
versusOf setup = Text.strip setup.versus

-- | A 19x19 game as Black against an opponent trying hard, which is
-- what a game of Go is when nobody says otherwise.
defaultSetup :: Setup
defaultSetup = Setup { against  = Computer
                     , size     = 19
                     , human    = Black
                     , strength = Strength 10
                     , handicap = 0
                     , speed    = Live
                     , handicapping = Standard
                       -- A rank belongs to the person who earned it,
                       -- and a program plays some of these games, so
                       -- nothing counts towards one unless it is asked
                       -- for.
                     , ranked   = False
                     , versus   = ""
                     }

-- | The boards on offer. Every board a player is likely to want, and
-- no more: the rules work on any width, and nobody plays 14x14.
boardWidths :: [Int]
boardWidths = [9, 13, 19]

-- | What the player did to the page.
data SetupEvent
  = ChosePlace Against
  | ChoseBoard Int
  | ChoseSide Color
  | ChoseStrength Strength
  | ChoseHandicap Int
  | ChoseSpeed Speed
  | ChoseRanked Bool
  | ChoseHandicapping Handicapping
  | ResumePressed Ongoing
    -- ^ Open a game this player is already in. Like signing in, this
    -- is about the window rather than about one tab.
  | RefreshPressed
    -- ^ Ask again which games this player is already in.
  | ChoseVersus Text
  | StartPressed
  | SignInPressed
    -- ^ Sign in to online-go.com. This is about the window rather than
    -- about one tab, and the window is what acts on it, the same way
    -- it acts on starting.
  deriving (Eq, Show)

-- | What a choice does to the page. Starting is not a choice, and the
-- window is what acts on it.
chose :: Setup -> SetupEvent -> Setup
chose setup = \case
  ChosePlace where'   -> setup { against = where' }
  ChoseBoard n        -> setup { size = n }
  ChoseSide side      -> setup { human = side }
  ChoseStrength which -> setup { strength = which }
  ChoseHandicap n     -> setup { handicap = n }
  ChoseSpeed how      -> setup { speed = how }
  ChoseRanked counts  -> setup { ranked = counts }
  ChoseHandicapping h -> setup { handicapping = h }
  ResumePressed _     -> setup
  RefreshPressed      -> setup
  ChoseVersus who     -> setup { versus = who }
  StartPressed        -> setup
  SignInPressed       -> setup

-- | The line under the title while this page is up.
describeSetup :: Setup -> Text
describeSetup setup = Text.intercalate "  \183  " (board : rest)
 where
  board = let n = Text.pack (show setup.size) in n <> "\215" <> n
  -- Only what that place asks for. A game somebody else pairs settles
  -- the colour and the opponent on the other side, so naming either
  -- here would be saying something this program does not know.
  rest  = case setup.against of
    -- Where, because a speed on its own could be anywhere, whereas a
    -- strength is a thing only a program has.
    Online -> [describePlace Online, describeSpeed setup.speed]
      <> [ "Ranked" | setup.ranked ]
      <> [ describeHandicapping setup.handicapping <> " handicap"
         | setup.handicapping /= Standard
         ]
      <> [ versusOf setup | not (Text.null (versusOf setup)) ]
    Computer ->
      [colorWord setup.human, "Level " <> describeStrength setup.strength]
        <> stones
  stones | setup.handicap < 2 = []
         | otherwise = [describeHandicap setup.handicap <> " stones"]

-- | The name of a colour, for a sentence.
colorWord :: Color -> Text
colorWord Black = "Black"
colorWord White = "White"

-- * The page
-------------

-- | The page a tab opens on.
--
-- An @AdwStatusPage@ is the widget for a window with nothing in it
-- yet. It carries the program's own icon, which is the one place a
-- player sees it from inside the program. Under it is a settings page
-- of the ordinary kind: a preferences group, a row per thing to
-- choose, and a toggle group in each row.
launchPage :: Signing -> [Ongoing] -> Setup -> Widget SetupEvent
launchPage signing already setup = bin
  Adw.StatusPage
  [ #iconName := "com.github.enzuru.Stones"
  , #title := "Stones"
  , #description := "Play Go"
  ]
  (bin
    Adw.Clamp
    [#maximumSize := 400]
    (container
      Gtk.Box
      [#orientation := Gtk.OrientationVertical, #spacing := 24]
      (  [ BoxChild defaultBoxChildProperties
           $ container Adw.PreferencesGroup [] (Vector.fromList rows)
         ]
      <> going
      <> [ BoxChild defaultBoxChildProperties $ widget
             Gtk.Button
             [ #label := button
             , #halign := Gtk.AlignCenter
             , #sensitive := ready
             , classes ["suggested-action", "pill"]
             , on #clicked StartPressed
             ]
         ]
      )
    )
  )
 where
  boardLabel n = let w = Text.pack (show n) in w <> "\215" <> w
  -- Ten of them, which is a list to drop down rather than a row of
  -- toggles: ten toggles side by side is a row too wide to read, and
  -- it made the whole page wider to hold it.
  levels = comboRow
    [#title := "Level"]
    defaultComboRowParams
      { choices  = Vector.fromList
        [ choice (describeStrength w) (describeStrength w) | w <- strengths ]
      , chosen   = Just (describeStrength setup.strength)
      , onChosen = Just (ChoseStrength . strengthNamed)
      }
  -- The games this player is already in, when there are any and there
  -- is somebody to be. Nothing has to be agreed to open one: it is
  -- already going, and this is only a way back into it.
  going
    | setup.against == Online && signedIn signing && not (null already)
    = [ BoxChild defaultBoxChildProperties $ container
          Adw.PreferencesGroup
          [ #title := "Your games"
          , headerSuffix
            (widget
              Gtk.Button
              [ #iconName := "view-refresh-symbolic"
              , #tooltipText := "Look again"
              , #valign := Gtk.AlignCenter
              , classes ["flat"]
              , on #clicked RefreshPressed
              ]
            )
          ]
          (Vector.fromList (map goingOn already))
      ]
    | otherwise
    = []
  -- One of them: who it is against, what it is, and a way in.
  goingOn :: Ongoing -> Widget SetupEvent
  goingOn one = container
    Adw.ActionRow
    [#title := one.with, #subtitle := describeSetup one.about]
    [ rowSuffix
        (widget
          Gtk.Button
          [ #label := "Open"
          , #valign := Gtk.AlignCenter
          , on #clicked (ResumePressed one)
          ]
        )
    ]
  board = toggling "Play on"
                 [ (Text.pack (show n), boardLabel n, n) | n <- boardWidths ]
                 setup.size
                 ChoseBoard
  where' = toggling "Play"
                  [ (describePlace p, describePlace p, p) | p <- places ]
                  setup.against
                  ChosePlace
  -- The page asks for what that place can answer, and for nothing
  -- else. A row that did nothing would be worse than no row at all.
  rows = case setup.against of
    Online ->
      [ where'
      , board
      , toggling "Speed"
               [ (describeSpeed how, describeSpeed how, how) | how <- speeds ]
               setup.speed
               ChoseSpeed
      , toggling "Rank"
               [("unranked", "Unranked", False), ("ranked", "Ranked", True)]
               setup.ranked
               ChoseRanked
      , toggling "Handicap"
               [ ( describeHandicapping h
                 , describeHandicapping h
                 , h
                 )
               | h <- handicappings
               ]
               setup.handicapping
               ChoseHandicapping
      , versus'
      , account
      ]
    Computer ->
      [ where'
      , board
      , toggling "You play"
               [ (colorWord side, colorWord side, side) | side <- [Black, White] ]
               setup.human
               ChoseSide
      , levels
      , toggling "Handicap"
               [ (describeHandicap n, describeHandicap n, n) | n <- handicaps ]
               setup.handicap
               ChoseHandicap
      ]
  -- The row shows what the page holds, and the page holds what was
  -- typed. A row that showed nothing while the page held a name would
  -- be a row that lied: the game would be asked for against somebody
  -- the player could not see.
  --
  -- Held rather than set. What the page holds is written back to the
  -- row on the render after every keystroke, and writing text into an
  -- entry puts the cursor at the front of it, so a row that were set
  -- on every keystroke is a row somebody types into backwards. Holding
  -- it reads the row first and writes nothing when the two already
  -- agree, which after a keystroke they do.
  versus' = widget
    Adw.EntryRow
    [ #title := "Opponent"
    , holding #text setup.versus
    , #tooltipText
      := "A name to play against, or nothing for whoever else is looking"
    , onM #changed (fmap ChoseVersus . Gtk.editableGetText)
    ]
  -- Signing in opens a browser, so it is a button rather than
  -- something that happens on its own when the place is chosen.
  account = container
    Adw.ActionRow
    [#title := "Account", #subtitle := describeSigning signing]
    [ rowSuffix
        (widget
          Gtk.Button
          [ #label := if signedIn signing then "Signed In" else "Sign In"
          , #valign := Gtk.AlignCenter
          , #sensitive := signing /= SigningIn && not (signedIn signing)
          , on #clicked SignInPressed
          ]
        )
    ]
  -- Nothing to start until there is somebody to start it as.
  ready = setup.against == Computer || signedIn signing
  button = case setup.against of
    Computer -> "Start Game"
    -- It is not started so much as waited for: somebody else has to be
    -- looking for the same game at the same time.
    Online   -> "Find a Game"

-- | One row of the page: what is being chosen, and a group of toggles
-- to choose from.
--
-- Each toggle carries a name, which is what the group answers with, so
-- the names and what they stand for are given together here and looked
-- up on the way back.
toggling
  :: Eq a
  => Text
  -> [(Text, Text, a)]
  -> a
  -> (a -> SetupEvent)
  -> Widget SetupEvent
toggling title options picked report = container
  Adw.ActionRow
  [#title := title]
  [ rowSuffix
      (toggleGroup
        [#valign := Gtk.AlignCenter]
        defaultToggleGroupParams
          { toggles     = Vector.fromList
            [ toggle name label | (name, label, _) <- options ]
          , active      = listToMaybe
            [ name | (name, _, value) <- options, value == picked ]
          , onActivated = Just (report . meaning)
          }
      )
  ]
 where
  -- A group answers with one of the names it was given, so the other
  -- way is not a way this goes. What is chosen now is what it answers
  -- with if it ever did, which is a choice that changes nothing.
  meaning name =
    fromMaybe picked (lookup name [ (n, value) | (n, _, value) <- options ])
