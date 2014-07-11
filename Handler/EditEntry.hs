module Handler.EditEntry
  ( getEditEntryR
  , postEditEntryR
  , entryForm )
where

import Import
import Data.Time.Clock
import Yesod.Form.Nic

entryForm :: RenderMessage App msg => msg -> Maybe Entry -> Form Entry
entryForm msg mentry = renderDivs $ Entry
    <$> areq textField (bfs MsgNewEntryTitle) (entryTitle <$> mentry)
    <*> maybe (lift (liftIO getCurrentTime)) (\entry -> pure $ entryPosted entry) mentry
    <*> areq nicHtmlField (bfs MsgNewEntryContent) (entryContent <$> mentry)
    <*  bootstrapSubmit (BootstrapSubmit msg "" [])

getEditEntryR :: EntryId -> Handler Html
getEditEntryR entryId = do
  entry <- runDB $ do
    entry <- get404 entryId
    return entry
  (entryWidget, enctype) <- generateFormPost (entryForm MsgSaveEntryChanges $ Just entry)
  defaultLayout $ do
    setTitleI MsgEditEntryTitle
    $(widgetFile "newentry")

postEditEntryR :: EntryId -> Handler Html
postEditEntryR entryId =  do
  oldEntry <- runDB $ do
    oldEntry <- get404 entryId
    return oldEntry
  ((res, entryWidget), enctype) <- runFormPost (entryForm MsgSaveEntryChanges $ Just oldEntry)
  case res of
    FormSuccess entry -> do
      runDB $ replace entryId entry
      setMessageI $ MsgEntryEdited $ entryTitle entry
      redirect $ EntryR entryId
    _ -> defaultLayout $ do
      setTitleI MsgPleaseCorrectEntry
      $(widgetFile "newentry")
