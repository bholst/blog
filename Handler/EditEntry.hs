module Handler.EditEntry
  ( getEditEntryR
  , postEditEntryR
  , entryForm
  , setErrorMessage )
where

import Import
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Time.Clock
import qualified Text.Blaze.Html5 as Html

entryForm :: RenderMessage App msg => msg -> Maybe (Entity Entry) -> Html -> MForm Handler (FormResult (Entry, Maybe [CategoryId]), Widget)
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
  let widget = $(widgetFile "edit-entry-form")
      entryRes = Entry <$> titleRes <*> postedTime <*> updatedTime <*> summaryRes <*> contentRes <* submitRes
  return ((,) <$> entryRes <*> categoriesRes, widget)
 where
  categoryCheckBox :: Entity Category -> (Text, CategoryId)
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
  entry <- runDB $ get404 entryId
  ((res, entryWidget), enctype) <-
    runFormPost $ entryForm MsgSaveEntryChanges (Just $ Entity entryId entry)
  isPreview <- runInputPost $ iopt boolField "preview"
  case res of
    FormSuccess (newentry, mcategories) ->
      case isPreview of
        Just True ->
          defaultLayout $ do
            setTitleI MsgEditEntryTitle
            $(widgetFile "previewentry")
        _ -> do
          runDB $ do
            deleteWhere [CategoryEntryEntry ==. entryId]
            replace entryId newentry
            case mcategories of
              Just categories ->
                mapM_ (\categoryId -> insert (CategoryEntry entryId categoryId)) categories
              Nothing ->
                return ()
          setMessageI $ MsgEntryEdited $ entryTitle newentry
          redirect $ EntryR entryId
    FormFailure texts -> do
      setErrorMessage texts
      defaultLayout $ do
        setTitleI MsgPleaseCorrectEntryTitle
        $(widgetFile "newentry")
    FormMissing -> do
      setMessageI $ MsgFormMissing
      defaultLayout $ do
        setTitleI MsgFormMissingTitle
        $(widgetFile "newentry")

setErrorMessage :: (RenderMessage App AppMessage) => [Text] -> Handler ()
setErrorMessage texts = do
  let errors = Html.ul $ mconcat (map (Html.li . toHtml) texts)
  mr <- getMessageRender
  setMessage $ (toHtml $ mr MsgPleaseCorrectEntry) `mappend` errors
