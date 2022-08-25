{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Score where

import Import

getScoreR :: Handler Html
getScoreR = do
  (uid, user) <- requireAuthPair
  users <- runDB $ selectList ([] :: [Filter User]) []
  scores <- mapM userScore users
  let userScores = sortWith snd $ zip users scores

  defaultLayout $ do
    $(widgetFile "score")
  where
    userScore u = do
      runDB $ count ([SubmissionUserId ==. entityKey u] :: [Filter Submission])
