module Handler.DeleteEntry where

import Import

deleteForm :: Html -> MForm Handler (FormResult (), Widget)
deleteForm extra = do
  (submitRes, submitView) <- mbootstrapSubmit (BootstrapSubmit MsgReallyDeleteSubmit "" [])
  let widget = $(widgetFile "delete-entry-form")
  return (submitRes, widget)

getDeleteEntryR :: EntryId -> Handler Html
getDeleteEntryR entryId = do
  entry <- runDB $ do
    entry <- get404 entryId
    return entry
  (formWidget, enctype) <- generateFormPost deleteForm
  defaultLayout $ do
    setTitleI MsgReallyDeleteEntryTitle
    $(widgetFile "delete-entry")

postDeleteEntryR :: EntryId -> Handler Html
postDeleteEntryR entryId = do
  ((res, _), _) <-
    runFormPost deleteForm
  case res of
    FormSuccess _ -> do
      runDB $ deleteCascade entryId
      setMessageI MsgEntryDeleted
      redirect   BlogR
    _ ->
      redirect $ EntryR entryId

