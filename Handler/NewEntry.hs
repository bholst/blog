module Handler.NewEntry
  ( getNewEntryR
  , postNewEntryR )
where

import Import
import Handler.EditEntry (entryForm)

getNewEntryR :: Handler Html
getNewEntryR = do
  (entryWidget, enctype) <- generateFormPost (entryForm MsgNewEntry Nothing)
  defaultLayout $ do
    setTitleI MsgNewEntryTitle
    $(widgetFile "newentry")

postNewEntryR :: Handler Html
postNewEntryR = do
  ((res, entryWidget), enctype) <- runFormPost (entryForm MsgNewEntry Nothing)
  case res of
    FormSuccess entry -> do
      entryId <- runDB $ insert entry
      setMessageI $ MsgEntryCreated $ entryTitle entry
      redirect $ EntryR entryId
    _ -> defaultLayout $ do
      setTitleI MsgPleaseCorrectEntry
      $(widgetFile "newentry")