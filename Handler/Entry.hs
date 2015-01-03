module Handler.Entry where

import Import
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


getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
    (entry, comments) <- runDB $ do
        entry <- get404 entryId
        comments <- selectList [CommentEntry ==. entryId] [Asc CommentPosted]
        return (entry, comments)
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
