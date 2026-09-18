{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The values a game of Go is made of.
module TypesTest
  ( tests
  )
where

import qualified Data.Text                     as Text
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen

import           Go.Types

colour :: Gen Color
colour = Gen.element [Black, White]

prop_theOtherPlayerIsTheOtherPlayer :: Property
prop_theOtherPlayerIsTheOtherPlayer = property $ do
  side <- forAll colour
  opposite (opposite side) === side
  assert (opposite side /= side)

prop_nobodyHasTakenAnythingToBeginWith :: Property
prop_nobodyHasTakenAnythingToBeginWith = property $ do
  side <- forAll colour
  capturedBy side noCaptures === 0

prop_capturesAddUpForTheOneWhoTookThem :: Property
prop_capturesAddUpForTheOneWhoTookThem = property $ do
  side <- forAll colour
  let after = addCapture side 3 (addCapture side 2 noCaptures)
  capturedBy side after === 5
  capturedBy (opposite side) after === 0

prop_takingNothingChangesNothing :: Property
prop_takingNothingChangesNothing = property $ do
  side <- forAll colour
  let before = addCapture Black 4 noCaptures
  addCapture side 0 before === before

prop_theTwoSidesAreCountedApart :: Property
prop_theTwoSidesAreCountedApart = withTests 1 . property $ do
  let both = addCapture White 2 (addCapture Black 7 noCaptures)
  blackCaptured both === 7
  whiteCaptured both === 2

prop_everyRefusalHasSomethingToSay :: Property
prop_everyRefusalHasSomethingToSay = property $ do
  reason <- forAll
    (Gen.element [OffBoard, Occupied, Suicide, KoRepeat, WrongPlayer, GameOver])
  let said = describeIllegal reason
  -- A sentence the player can read, rather than the name of a
  -- constructor.
  assert (not (Text.null said))
  assert (Text.isSuffixOf "." said)
  assert (said /= Text.pack (show reason))

prop_refusalsDoNotShareAWordingWithEachOther :: Property
prop_refusalsDoNotShareAWordingWithEachOther = withTests 1 . property $ do
  let every' =
        [OffBoard, Occupied, Suicide, KoRepeat, WrongPlayer, GameOver]
      said = map describeIllegal every'
  length said === length (uniqueOf said)
  where uniqueOf = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

tests :: Group
tests = $$(discover)
