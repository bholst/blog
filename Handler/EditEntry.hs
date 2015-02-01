module Handler.EditEntry
  ( getEditEntryR
  , postEditEntryR
  , entryForm
  , handleEntryFormResult )
where

import Import
import Control.Monad (liftM)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as Text
import Data.Time.Clock
import qualified Handler.Entry as Entry (entryWidget)
import qualified Text.Blaze.Html5 as Html

entryForm :: RenderMessage App msg => msg -> Maybe (Entity Entry) -> Html -> MForm Handler (FormResult (Entry, Maybe [CategoryId], Maybe [UploadId]), Widget)
entryForm msg mentryEntity extra = do
  let mentry = entityVal <$> mentryEntity
  (categories, categoryEntries) <- lift $ runDB $ do
    categories <- selectList [] [Asc CategoryName]
    case mentryEntity of
      Just (Entity entryId _) -> do
        categoryEntries <- selectList [CategoryEntryEntry ==. entryId] []
        return (categories, Just $ Just (map (categoryEntryCategory . entityVal) categoryEntries))
      Nothing ->
        return (categories, Nothing)
  (uploads, entryUploads) <- lift $ runDB $ do
    uploads <- selectList [] [Asc UploadDescription]
    case mentryEntity of
      Just (Entity entryId _) -> do
        entryUploads <- selectList [EntryUploadEntry ==. entryId] []
        return (uploads, Just $ Just (map (entryUploadUpload . entityVal) entryUploads))
      Nothing ->
        return (uploads, Nothing)
  (titleRes, titleView) <- mreq textField (bfs MsgNewEntryTitle) (entryTitle <$> mentry)
  currentTime <- liftIO getCurrentTime
  extraSettings <- lift getExtra
  let maybeMaxLength = summaryMaxLength extraSettings
      validateSummary summary =
        case maybeMaxLength of
          Just maxLength
            | Text.length summary > maxLength ->
              Left (MsgSummaryTooLong maxLength)
          _ -> Right summary
      summaryField =
        check validateSummary textField
  (summaryRes, summaryView) <- mreq summaryField (bfs MsgNewEntrySummary) (entrySummary <$> mentry)
  let postedTime  = pure $ maybe currentTime entryPosted mentry
      updatedTime = pure currentTime
      cfs        = (bfs MsgNewEntryContent) :: FieldSettings App
      attributes = ("rows", "20") : fsAttrs cfs
      newCfs     = cfs { fsAttrs = attributes }
  (contentRes, contentView) <- mreq markdownField newCfs (entryContent <$> mentry)
  (submitRes, submitView) <- mbootstrapSubmit (BootstrapSubmit msg "" [])
  (categoriesRes, checkBoxView) <- mopt (checkboxesFieldList $ map categoryCheckBox categories) (bfs MsgNewEntryCategories) categoryEntries
  (uploadsRes, uploadsView) <- mopt (multiSelectFieldList $ map uploadsOption uploads) (bfs MsgNewEntryImages) entryUploads
  let widget = $(widgetFile "edit-entry-form")
      entryRes = Entry <$> titleRes <*> postedTime <*> updatedTime <*> summaryRes <*> contentRes <* submitRes
  return ((,,) <$> entryRes <*> categoriesRes <*> uploadsRes, widget)
 where
  categoryCheckBox :: Entity Category -> (Text, CategoryId)
  categoryCheckBox (Entity categoryId category) =
    ((categoryName category), categoryId)

  uploadsOption :: Entity Upload -> (Text, UploadId)
  uploadsOption (Entity uploadId upload) =
    ((uploadDescription upload), uploadId)

handleEntryFormResult :: Maybe EntryId -> ((FormResult (Entry, Maybe [CategoryId], Maybe [UploadId]), Widget), Enctype) -> Handler Html
handleEntryFormResult mEntryId ((FormSuccess (newentry, mcategories, muploadIds), entryWidget), enctype) = do
  let uploadIds = fromMaybe [] muploadIds
      categories = fromMaybe [] mcategories
  isPreview <- runInputPost $ iopt boolField "preview"
  case isPreview of
    Just True -> do
      uploads <-
        liftM catMaybes $ runDB $
        mapM (\eid -> get eid >>= return . fmap (Entity eid)) uploadIds
      muser <- maybeAuth
      defaultLayout $ do
        setTitleI MsgEditEntryTitle
        $(widgetFile "previewentry")
    _ -> do
      entryId <-
        runDB $ do
          entryId <-
            case mEntryId of
              Just entryId -> do
                deleteWhere [CategoryEntryEntry ==. entryId]
                deleteWhere [EntryUploadEntry ==. entryId]
                replace entryId newentry
                return entryId
              Nothing ->
                insert newentry
          mapM_ (\categoryId -> insert (CategoryEntry entryId categoryId)) categories
          mapM_ (\uploadId -> insert (EntryUpload entryId uploadId)) uploadIds
          return entryId
      setMessageI $ MsgEntryEdited $ entryTitle newentry
      redirect $ EntryR entryId
handleEntryFormResult _ (((FormFailure texts), entryWidget), enctype) = do
  setErrorMessage texts
  defaultLayout $ do
    setTitleI MsgPleaseCorrectEntryTitle
    $(widgetFile "newentry")
handleEntryFormResult _ ((FormMissing, entryWidget), enctype) = do
  setMessageI $ MsgFormMissing
  defaultLayout $ do
    setTitleI MsgFormMissingTitle
    $(widgetFile "newentry")

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
  entry <- runDB $ get404 entryId
  formResult <-
    runFormPost $ entryForm MsgSaveEntryChanges (Just $ Entity entryId entry)
  handleEntryFormResult (Just entryId) formResult

setErrorMessage :: (RenderMessage App AppMessage) => [Text] -> Handler ()
setErrorMessage texts = do
  let errors = Html.ul $ mconcat (map (Html.li . toHtml) texts)
  mr <- getMessageRender
  setMessage $ (toHtml $ mr MsgPleaseCorrectEntry) `mappend` errors
