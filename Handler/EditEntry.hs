module Handler.EditEntry
  ( getEditEntryR
  , postEditEntryR
  , entryForm
  , handleEntryFormResult )
where

import Import
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as Text
import Data.Time.Clock
import qualified Handler.Entry as Entry (entryWidget, getUploads)
import qualified Text.Blaze.Html5 as Html

entryForm :: RenderMessage App msg => msg -> Maybe (Entity Entry) -> Html -> MForm Handler (FormResult (Entry, Maybe [CategoryId], Maybe [UploadId]), Widget)
entryForm msg mentryEntity extra = do
  let mentry = entityVal <$> mentryEntity
      mentryId = entityKey <$> mentryEntity
  categoriesWithEntries <- lift $ runDB $
    select $ from $ \(categoryEntry `RightOuterJoin` category) -> do
      on $
        (just (category ^. CategoryId) ==. categoryEntry ?. CategoryEntryCategory)
        &&. (categoryEntry ?. CategoryEntryEntry ==. val mentryId)
      return (category, categoryEntry)
  let categories = map fst categoriesWithEntries
      categoryEntries =
        case mentryId of
          Nothing -> Nothing
          Just _ ->
            Just $ Just $
            map (entityKey . fst) $ filter (\(_, ce) -> isJust ce)
            categoriesWithEntries
  uploadsWithEntries <- lift $ runDB $
    select $ from $ \(entryUpload `RightOuterJoin` upload) -> do
      on $
        (just (upload ^. UploadId) ==. entryUpload ?. EntryUploadUpload)
        &&. (entryUpload ?. EntryUploadEntry ==. val mentryId)
      return (upload, entryUpload)
  let uploads = map fst uploadsWithEntries
      entryUploads =
         case mentryId of
           Nothing -> Nothing
           Just _ ->
             Just $ Just $
             map (entityKey . fst) $ filter (\(_, eu) -> isJust eu)
             uploadsWithEntries
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
  (titleImageRes, titleImageView) <- mopt (selectFieldList $ map uploadsOption uploads) (bfs MsgNewEntryTitleImage) (entryTitleImage <$> mentry)
  (uploadsRes, uploadsView) <- mopt (multiSelectFieldList $ map uploadsOption uploads) (bfs MsgNewEntryImages) entryUploads
  let widget = $(widgetFile "edit-entry-form")
      entryRes = Entry <$> titleRes <*> postedTime <*> updatedTime <*> summaryRes <*> contentRes <*> titleImageRes <* submitRes
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
        runDB $
        Entry.getUploads newentry $
        liftM catMaybes $
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
                delete $ from (\ce -> where_ (ce ^. CategoryEntryEntry ==. val entryId))
                delete $ from (\eu -> where_ (eu ^. EntryUploadEntry ==. val entryId))
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
