module Handler.NewEntry
  ( getNewEntryR
  , postNewEntryR )
where

import Import
import Handler.EditEntry (entryForm)

getNewEntryR :: Handler Html
getNewEntryR = do
  (entryWidget, enctype) <-
    generateFormPost $ entryForm MsgNewEntry Nothing
  defaultLayout $ do
    setTitleI MsgNewEntryTitle
    $(widgetFile "newentry")

postNewEntryR :: Handler Html
postNewEntryR = do
  ((res, entryWidget), enctype) <-
    runFormPost $ entryForm MsgNewEntry Nothing
  case res of
    FormSuccess (entry, mcategories) -> do
      entryId <- runDB $ do
        entryId <- insert entry
        case mcategories of
          Just categories ->
            mapM_ (\categoryId -> insert (CategoryEntry entryId categoryId))
                  categories
          Nothing -> return ()
        return entryId
      setMessageI $ MsgEntryCreated $ entryTitle entry
      redirect $ EntryR entryId
    failure -> defaultLayout $ do
      case failure of
        FormFailure texts -> setMessage $ toHtml $ show texts
        FormMissing -> setMessage $ toHtml ("FormMissing" :: Text)
        _  -> setMessage $ toHtml ("Other error" :: Text)
      setTitleI MsgPleaseCorrectEntry
      $(widgetFile "newentry")
