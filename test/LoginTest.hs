-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Signing in to online-go.com.
--
-- Nothing here reaches online-go.com. The browser and the trade at the
-- end are both arguments to the flow, so the test plays the browser
-- itself, against the socket the flow opens on this machine.
module LoginTest
  ( tests
  )
where

import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.MVar
import           Control.Monad                  ( void )
import qualified Data.ByteString.Lazy          as Lazy
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import qualified Network.HTTP.Client           as Http

import           Stones.Ogs
import           Stones.Ogs.Login

-- | The example in the specification that describes this, which is
-- the one number in here that somebody else decided.
rfcVerifier :: Text
rfcVerifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"

rfcChallenge :: Text
rfcChallenge = "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"

prop_theChallengeIsTheOneTheSpecificationGives :: Property
prop_theChallengeIsTheOneTheSpecificationGives = withTests 1 . property $ do
  challengeFor rfc === rfcChallenge

prop_aFreshVerifierIsOneTheRulesAccept :: Property
prop_aFreshVerifierIsOneTheRulesAccept = property $ do
  Verifier made <- evalIO newVerifier
  case verifierFrom made of
    Left  why -> annotate (Text.unpack why) >> failure
    Right _   -> success
  -- And a second one is not the first. A verifier that repeated would
  -- be no secret at all.
  Verifier again <- evalIO newVerifier
  assert (made /= again)

prop_aVerifierHasToBeLongEnoughAndPlainEnough :: Property
prop_aVerifierHasToBeLongEnoughAndPlainEnough = withTests 1 . property $ do
  assert (isLeft (verifierFrom (Text.replicate 42 "a")))
  assert (isLeft (verifierFrom (Text.replicate 129 "a")))
  assert (isLeft (verifierFrom (Text.replicate 42 "a" <> "/")))
  assert (isRight (verifierFrom (Text.replicate 43 "a")))
  assert (isRight (verifierFrom (Text.replicate 128 "a")))
  -- The four that are not letters or numbers.
  assert (isRight (verifierFrom (Text.replicate 39 "a" <> "-._~")))

-- | The example verifier, as a verifier.
rfc :: Verifier
rfc = either (error . Text.unpack) id (verifierFrom rfcVerifier)

-- | The parameters of an address.
askedBy :: Text -> [(Text, Text)]
askedBy address =
  parameters (Text.drop 1 (Text.dropWhile (/= '?') address))

prop_theAddressCarriesEverythingTheServerNeeds :: Property
prop_theAddressCarriesEverythingTheServerNeeds =
  withTests 1 . property $ do
    let redirect = redirectFor 41414
        asked    = askedBy (authorizeFor redirect rfc "a-state")
    lookup "response_type" asked === Just "code"
    lookup "client_id" asked === Just clientId
    lookup "redirect_uri" asked === Just redirect
    lookup "state" asked === Just "a-state"
    lookup "code_challenge" asked === Just rfcChallenge
    lookup "code_challenge_method" asked === Just "S256"

prop_whatIsEscapedComesBackTheSame :: Property
prop_whatIsEscapedComesBackTheSame = property $ do
  -- The address is built by escaping and read by unescaping, and a
  -- redirect with a colon and slashes in it goes through both.
  value <- forAll (Gen.text (Range.linear 0 24) Gen.unicode)
  -- An escape stands for a byte, and a letter can be several of them,
  -- so this is the test that says they are put back together.
  lookup "state" (askedBy (authorizeFor (redirectFor 41414) rfc value))
    === Just value

prop_theCodeIsTakenOutOfWhatTheBrowserAsksFor :: Property
prop_theCodeIsTakenOutOfWhatTheBrowserAsksFor =
  withTests 1 . property $ do
    codeFrom
        "the-state"
        "GET /callback?code=a-code&state=the-state HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"
      === Right "a-code"

prop_anAnswerToSomebodyElsesQuestionIsRefused :: Property
prop_anAnswerToSomebodyElsesQuestionIsRefused =
  withTests 1 . property $ do
    -- The state is the only thing that says this request is the answer
    -- to the one this program started, rather than something else that
    -- found the socket first.
    assert
      (isLeft
        (codeFrom "the-state" "GET /callback?code=a-code&state=another HTTP/1.1\r\n")
      )
    assert (isLeft (codeFrom "the-state" "GET /callback?code=a-code HTTP/1.1\r\n"))
    assert (isLeft (codeFrom "the-state" "GET /callback?state=the-state HTTP/1.1\r\n"))
    assert (isLeft (codeFrom "the-state" "nonsense\r\n"))

prop_aRefusalFromTheServerIsPassedOn :: Property
prop_aRefusalFromTheServerIsPassedOn = withTests 1 . property $ do
  case codeFrom
      "the-state"
      "GET /callback?error=access_denied&error_description=You+said+no&state=the-state HTTP/1.1\r\n"
    of
      Left why -> assert (Text.isInfixOf "You said no" why)
      Right _  -> annotate "it took a refusal for a code" >> failure

prop_whatTheTokenEndpointSaysIsRead :: Property
prop_whatTheTokenEndpointSaysIsRead = withTests 1 . property $ do
  tokensFrom
      "{\"access_token\":\"abc\",\"refresh_token\":\"def\",\"expires_in\":36000,\"token_type\":\"Bearer\"}"
    === Right Tokens { access = "abc", refresh = Just "def", expires = Just 36000 }
  -- The two that are not promised.
  tokensFrom "{\"access_token\":\"abc\"}"
    === Right Tokens { access = "abc", refresh = Nothing, expires = Nothing }
  assert (isLeft (tokensFrom "{\"error\":\"invalid_grant\"}"))
  assert (isLeft (tokensFrom "not json"))

prop_theWholeDanceWithoutReachingTheServer :: Property
prop_theWholeDanceWithoutReachingTheServer = withTests 1 . property $ do
  -- The flow opens a socket on this machine and waits. The browser
  -- here is the test, which reads the address, goes to it, and is
  -- answered with the page a person would see.
  (outcome, shown) <- evalIO $ do
    page <- newEmptyMVar
    let browser address = void . forkIO $ putMVar page =<< visiting address
    answer <- loginWith browser pretending
    (,) answer <$> takeMVar page
  case outcome of
    Left  why    -> annotate (Text.unpack why) >> failure
    -- What the fake trade does with the code is hand it back, so this
    -- says the code the browser brought reached the end.
    Right tokens -> tokens.access === "a-code"
  assert (Text.isInfixOf "Signed in" shown)

-- | A trade that reaches nothing and answers with the code it was
-- given, so that a test can see the code came through.
pretending :: Text -> Verifier -> Text -> IO (Either Text Tokens)
pretending _redirect _verifier code =
  pure (Right Tokens { access = code, refresh = Nothing, expires = Nothing })

-- | Do what a browser does: read the address, go where it says to go
-- afterwards, and answer with the page that comes back.
visiting :: Text -> IO Text
visiting address = do
  let asked = askedBy address
  case (lookup "redirect_uri" asked, lookup "state" asked) of
    (Just redirect, Just state) -> do
      manager <- Http.newManager Http.defaultManagerSettings
      request <- Http.parseRequest
        (Text.unpack (redirect <> "?code=a-code&state=" <> state))
      answer <- Http.httpLbs request manager
      pure (Text.decodeUtf8 (Lazy.toStrict (Http.responseBody answer)))
    _ -> pure "the address had nothing to go back to in it"

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

isRight :: Either a b -> Bool
isRight = not . isLeft

tests :: Group
tests = $$(discover)
