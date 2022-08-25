{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = do
  (uid, user) <- requireAuthPair
  comps <- runDB $ selectList ([] :: [Filter Competition]) []

  subs <- runDB $ selectList ([SubmissionUserId ==. uid] :: [Filter Submission]) []
  chals' <- mapM getSubChallenge subs
  let chals = concat chals'

  (compWidget, compEncType) <- generateFormPost competitionCreationForm
  defaultLayout $ do
    setTitle . toHtml $ userIdent user <> "'s User page"
    $(widgetFile "profile")
  where
    getSubChallenge s = do
      runDB $ selectList ([ChallengeId ==. submissionChallengeId (entityVal s)] :: [Filter Challenge]) []

postProfileR :: Handler Html
postProfileR = do
  (_, user) <- requireAuthPair
  ((result, _), _) <- runFormPost competitionCreationForm
  _ <- if userAdmin user then createAction result else return Nothing
  redirect ProfileR
  where
    createAction r =
      case r of
        FormSuccess c -> do
          entity <- runDB $ insertEntity $ Competition (name c) (unTextarea $ description c) Nothing Nothing
          return $ Just entity
        _ -> return Nothing

data CompetitionForm = CompetitionForm
  { name :: Text,
    description :: Textarea
  }

competitionCreationForm :: Form CompetitionForm
competitionCreationForm =
  renderDivs $
    CompetitionForm
      <$> areq textField "Name" Nothing
      <*> areq textareaField "Description" Nothing
