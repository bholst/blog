module Handler.NewEntry
  ( getNewEntryR
  , postNewEntryR )
where

import Import
import Handler.EditEntry (entryForm, handleEntryFormResult)

getNewEntryR :: Handler Html
getNewEntryR = do
  (entryWidget, enctype) <-
    generateFormPost $ entryForm MsgNewEntry Nothing
  defaultLayout $ do
    setTitleI MsgNewEntryTitle
    $(widgetFile "newentry")

postNewEntryR :: Handler Html
postNewEntryR = do
  formResult <-
    runFormPost $ entryForm MsgNewEntry Nothing
  handleEntryFormResult Nothing formResult

