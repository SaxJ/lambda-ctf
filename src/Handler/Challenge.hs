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
  challenge <- runDB $ get404 cid
  flags <- runDB $ selectList [FlagChallengeId ==. cid] []
  ((submission, _), _) <- runFormPost flagSubmissionForm
  (flagWidget, encType) <- generateFormPost flagSubmissionForm

  let goodSubmission = checkResult submission flags
  let msg = if goodSubmission then Just "Correct" :: Maybe String else Just "Incorrect"
  Control.Monad.when goodSubmission $ do
    _ <- runDB $ insert $ Submission uid cid
    return ()

  defaultLayout
    $(widgetFile "challenge")
  where
    checkResult r fs = case r of
      FormSuccess f -> formFlag f `elem` map (flagValue . entityVal) fs
      _ -> False

data FlagSubmissionForm = FlagSubmissionForm
  { formFlag :: Text
  }

flagSubmissionForm :: Form FlagSubmissionForm
flagSubmissionForm =
  renderDivs $
    FlagSubmissionForm
      <$> areq textField "Flag" Nothing

