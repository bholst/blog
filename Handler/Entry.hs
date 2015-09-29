module Handler.Entry where

import Import
import qualified Data.List as L
import qualified Data.Text as Text
import Data.Time.Clock

commentForm :: EntryId -> Form Comment
commentForm entryId = renderDivs $ Comment
    <$> pure entryId
    <*> lift (liftIO getCurrentTime)
    <*> lift requireAuthId
    <*> areq textField (bfs MsgCommentName) Nothing
    <*> areq textareaField (bfs MsgCommentText) Nothing
    <*  bootstrapSubmit (BootstrapSubmit MsgAddCommentButton "" [])

canDeleteCommentBy :: Maybe (Entity User) -> UserId -> Bool
canDeleteCommentBy muser commentUserId =
  case muser of
    Nothing -> False
    Just (Entity currentUserId currentUser) ->
      if isAdmin currentUser
        then True
        else currentUserId == commentUserId

entryWidget :: Entry -> [Entity Upload] -> Maybe (Entity User) -> Maybe EntryId -> Maybe (Route App) -> Widget
entryWidget entry uploads muser mEntryId mLink =
  let (images, otherUploads) = L.partition isImage uploads
      indexedImages = zip ([0..] :: [Integer]) images
  in $(widgetFile "entry-widget")
  where
    isImage :: Entity Upload -> Bool
    isImage (entityVal -> uploadEntity) =
      case Text.takeWhile (/= '/') $ uploadType uploadEntity of
        "image" -> True
        _ -> False

getOtherUploads :: EntryId -> SqlPersistT Handler [Entity Upload]
getOtherUploads entryId =
  select $
  from $ \(entryUpload `InnerJoin` upload) -> do
    where_ (entryUpload ^. EntryUploadEntry ==. val entryId)
    on (entryUpload ^. EntryUploadUpload ==. upload ^. UploadId)
    return upload

getUploads :: Entry -> SqlPersistT Handler [Entity Upload] -> SqlPersistT Handler [Entity Upload]
getUploads entry getOthers = do
    others <- getOthers
    mTitleImage <-
      case entryTitleImage entry of
        Nothing -> return Nothing
        Just imageId -> do
          image <- get imageId
          return $ fmap (Entity imageId) image
    case mTitleImage of
      Nothing -> return $ others
      Just titleImage ->
        return $ titleImage : (filter (\e -> entityKey e /= entityKey titleImage) others)

getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
    (entry, comments, uploads) <- runDB $ do
        entry <- get404 entryId
        comments <-
          select $
          from $ \comment -> do
            where_ (comment ^. CommentEntry ==. val entryId)
            orderBy [asc (comment ^. CommentPosted)]
            return comment
        uploads <- getUploads entry $ getOtherUploads entryId
        return (entry, comments, uploads)
    muser <- maybeAuth
    mCommentWidget <-
      case muser of
        Nothing -> return Nothing
        Just _  -> generateFormPost (commentForm entryId) >>= return . Just
    extra <- getExtra
    let pagename = extraPagename extra
    defaultLayout $ do
        setTitleI $ MsgEntryTitle pagename $ entryTitle entry
        $(widgetFile "entry")

postEntryR :: EntryId -> Handler Html
postEntryR entryId = do
    extra <- getExtra
    if extraEnableComments extra
      then do
        ((res, commentWidget), enctype) <-
            runFormPost (commentForm entryId)
        case res of
            FormSuccess comment -> do
                _ <- runDB $ insert comment
                setMessageI MsgCommentAdded
                redirect $ EntryR entryId
            _ -> defaultLayout $ do
                setTitleI MsgPleaseCorrectComment
                $(widgetFile "newcomment")
      else
        defaultLayout $ do
          setMessageI MsgCommentsDisabled
          redirect $ EntryR entryId
