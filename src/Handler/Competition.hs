{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Competition where

import Import

getCompetitionR :: CompetitionId -> Handler Html
getCompetitionR cid = do
  (_, user) <- requireAuthPair
  competition <- runDB $ get cid
  challenges <- runDB $ selectList (competitionChallenges cid) []
  case competition of
    Just comp -> defaultLayout $ do
      $(widgetFile "competition")
    Nothing -> notFound

getCompetitionAdminR :: CompetitionId -> Handler Html
getCompetitionAdminR cid = do
  competition <- runDB $ get cid
  challenges <- runDB $ selectList (competitionChallenges cid) []
  (widget, encType) <- generateFormPost challengeCreationForm
  case competition of
    Just comp -> defaultLayout $ do
      $(widgetFile "competition_admin")
    Nothing -> notFound

postCompetitionAdminR :: CompetitionId -> Handler Html
postCompetitionAdminR cid = do
  (_, user) <- requireAuthPair
  ((result, _), _) <- runFormPost challengeCreationForm
  _ <- if userAdmin user then createAction result else return Nothing
  redirect $ CompetitionAdminR cid
  where
    createAction r =
      case r of
        FormSuccess c -> do
          entity <- runDB $ insertEntity $ Challenge (challengeFormName c) (unTextarea $ challengeFormInformation c) cid 1 (challengeFormFlags c)
          return $ Just entity
        _ -> return Nothing

postCompetitionStartR :: CompetitionId -> Handler Html
postCompetitionStartR cid = do
  ct <- liftIO getCurrentTime
  runDB $ do update cid [CompetitionStarted =. Just ct]
  redirect ProfileR

postCompetitionEndR :: CompetitionId -> Handler Html
postCompetitionEndR cid = do
  ct <- liftIO getCurrentTime
  runDB $ do update cid [CompetitionEnded =. Just ct]
  redirect ProfileR

competitionChallenges :: CompetitionId -> [Filter Challenge]
competitionChallenges cid = [ChallengeComptitionId ==. cid]

data ChallengeForm = ChallengeForm
  { challengeFormName :: Text,
    challengeFormInformation :: Textarea,
    challengeFormFlags :: Text
  }

challengeCreationForm :: Form ChallengeForm
challengeCreationForm =
  renderDivs $
    ChallengeForm
      <$> areq textField "Name" Nothing
      <*> areq textareaField "Description" Nothing
      <*> areq textField "Flags (Pipe Separated)" Nothing
