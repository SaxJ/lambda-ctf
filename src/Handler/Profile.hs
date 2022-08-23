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
  (_, user) <- requireAuthPair
  (compWidget, compEncType) <- generateFormPost competitionCreationForm
  profileLayout user compWidget compEncType Nothing

postProfileR :: Handler Html
postProfileR = do
  (_, user) <- requireAuthPair
  ((result, formWidget), formEncType) <- runFormPost competitionCreationForm
  created <- case result of
    FormSuccess c -> do
      entity <- runDB $ insertEntity $ Competition (name c) (unTextarea $ description c) Nothing Nothing
      return $ Just entity
    _ -> return Nothing
  profileLayout user formWidget formEncType created

profileLayout user compWidget compEncType mCreated = defaultLayout $ do
  setTitle . toHtml $ userIdent user <> "'s User page"
  $(widgetFile "profile")

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
