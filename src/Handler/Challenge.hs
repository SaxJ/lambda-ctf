{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Handler.Challenge where

import qualified Control.Monad
import Yesod.Markdown
import Import
import Data.Either (fromRight)
import qualified Network.HTTP.Simple as HTTP
import Data.Aeson (defaultOptions)
import Data.Aeson.Types (genericToEncoding)
import System.Environment (getEnv)

renderedChallengeInformation :: Maybe Challenge -> Html
renderedChallengeInformation mc = info
  where
      info = case mc of
        Just c -> fromRight "" $ markdownToHtml $ Markdown $ challengeInformation c
        Nothing -> ""

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
  app <- getYesod
  (uid, user') <- requireAuthPair
  challenge <- runDB $ get404 cid
  flags <- runDB $ selectList [FlagChallengeId ==. cid] []
  ((submission, _), _) <- runFormPost flagSubmissionForm
  (flagWidget, encType) <- generateFormPost flagSubmissionForm

  let goodSubmission = checkResult submission flags
  let msg = if goodSubmission then Just "Correct" :: Maybe String else Just "Incorrect"
  Control.Monad.when goodSubmission $ do
    _ <- makeSlackRequest app (challengeName challenge) (userName user')
    _ <- runDB $ insert $ Submission uid cid
    return ()

  defaultLayout
    $(widgetFile "challenge")
  where
    checkResult r fs = case r of
      FormSuccess f -> formFlag f `elem` map (flagValue . entityVal) fs
      _ -> False

makeSlackRequest :: (MonadIO m, MonadLogger m) => App -> Text -> Text -> m (Response ())
makeSlackRequest app c u = do
  hook <- liftIO $ getEnv "SLACK_WEBHOOK"
  let body = SlackBody u c
  initReq <- liftIO $ HTTP.parseRequest hook
  let req = HTTP.setRequestBodyJSON body $ initReq {method = "POST"}
  HTTP.httpNoBody req

data SlackBody = SlackBody
  { user :: Text
  , challenge :: Text
  } deriving (Generic, Show)
instance ToJSON SlackBody where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON SlackBody

data FlagSubmissionForm = FlagSubmissionForm
  { formFlag :: Text
  }

flagSubmissionForm :: Form FlagSubmissionForm
flagSubmissionForm =
  renderDivs $
    FlagSubmissionForm
      <$> areq textField "Flag" Nothing

