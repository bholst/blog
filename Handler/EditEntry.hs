module Handler.EditEntry
  ( getEditEntryR
  , postEditEntryR
  , entryForm )
where

import Import hiding (mapM)
import Data.Time.Clock
import Data.Traversable (mapM)

required :: FieldView app -> Text
required fv = case fvRequired fv of
  True -> "required"
  False -> "optional"

entryForm :: RenderMessage App msg => msg -> Maybe (Entity Entry) -> Html -> MForm Handler (FormResult (Entry, [CategoryId]), Widget)
entryForm msg mentryEntity extra = do
  let mentry = entityVal <$> mentryEntity
  (categories, categoryEntries) <- lift $ runDB $ do
    categories <- selectList [] [Asc CategoryName]
    categoryEntries <-
      mapM (\(Entity entryId _) -> selectList [CategoryEntryEntry ==. entryId] []) mentryEntity
    return (categories, (map (categoryEntryCategory . entityVal)) <$> categoryEntries)
  (titleRes, titleView) <- mreq textField (bfs MsgNewEntryTitle) (entryTitle <$> mentry)
  currentTime <- liftIO getCurrentTime
  let time = pure $ maybe currentTime entryPosted mentry
      cfs        = (bfs MsgNewEntryContent) :: FieldSettings App
      attributes = ("rows", "20") : fsAttrs cfs
      newCfs     = cfs { fsAttrs = attributes }
  (contentRes, contentView) <- mreq markdownField newCfs (entryContent <$> mentry)
  (submitRes, submitView) <- mbootstrapSubmit (BootstrapSubmit msg "" [])
  (categoriesRes, checkBoxView) <- mreq (checkboxesFieldList $ map categoryCheckBox categories) (bfs MsgNewEntryCategories) categoryEntries
  let widget = $(widgetFile "edit-entry-form")
      entryRes = Entry <$> titleRes <*> time <*> contentRes <* submitRes
  return ((,) <$> entryRes <*> categoriesRes, widget)
 where
  categoryCheckBox (Entity categoryId category) =
    ((categoryName category), categoryId)

getEditEntryR :: EntryId -> Handler Html
getEditEntryR entryId = do
  entry <- runDB $ get404 entryId
  (entryWidget, enctype) <-
    generateFormPost $ entryForm MsgSaveEntryChanges (Just $ Entity entryId entry)
  defaultLayout $ do
    setTitleI MsgEditEntryTitle
    $(widgetFile "newentry")

postEditEntryR :: EntryId -> Handler Html
postEditEntryR entryId =  do
  ((res, entryWidget), enctype) <-
    runFormPost $ entryForm MsgSaveEntryChanges Nothing
  case res of
    FormSuccess (entry, categories) -> do
      runDB $ do
        deleteWhere [CategoryEntryEntry ==. entryId]
        replace entryId entry
        mapM_ (\categoryId -> insert (CategoryEntry entryId categoryId))
              categories
      setMessageI $ MsgEntryEdited $ entryTitle entry
      redirect $ EntryR entryId
    _ -> defaultLayout $ do
      setTitleI MsgPleaseCorrectEntry
      $(widgetFile "newentry")
