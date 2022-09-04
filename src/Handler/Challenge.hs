{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Handler.Challenge where

import qualified Control.Monad
import Yesod.Markdown
import Import
import Data.Either (fromRight)

renderedChallengeInformation :: Maybe Challenge -> Html
renderedChallengeInformation mc = info
  where
      info = case mc of
        Just challenge -> fromRight "" $ markdownToHtml $ Markdown $ challengeInformation challenge
        _ -> ""

getChallengeR :: ChallengeId -> Handler Html
getChallengeR cid = do
  let msg = Nothing :: Maybe String
  mChallenge <- runDB $ get cid
  (flagWidget, encType) <- generateFormPost flagSubmissionForm
  case mChallenge of
    Just challenge ->
      defaultLayout
        $(widgetFile "challenge")
    Nothing -> notFound

postChallengeR :: ChallengeId -> Handler Html
postChallengeR cid = do
  (uid, _) <- requireAuthPair
  mChallenge <- runDB $ get cid
  ((result, _), _) <- runFormPost flagSubmissionForm
  (flagWidget, encType) <- generateFormPost flagSubmissionForm

  let goodSubmission = checkResult result mChallenge
  let msg = if goodSubmission then Just "Correct" :: Maybe String else Just "Incorrect"
  Control.Monad.when goodSubmission $ do
    _ <- runDB $ insert $ Submission uid cid
    return ()

  case mChallenge of
    Just challenge ->
      defaultLayout
        $(widgetFile "challenge")
    Nothing -> notFound
  where
    checkResult r mc = case r of
      FormSuccess f -> case mc of
        Just c -> formFlag f `isInfixOf` challengeFlags c
        _ -> False
      _ -> False

data FlagSubmissionForm = FlagSubmissionForm
  { formFlag :: Text
  }

flagSubmissionForm :: Form FlagSubmissionForm
flagSubmissionForm =
  renderDivs $
    FlagSubmissionForm
      <$> areq textField "Flag" Nothing
