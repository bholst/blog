module Handler.DeleteComment
  ( getDeleteCommentR
  , postDeleteCommentR )
where

import Import

deleteForm :: Html -> MForm Handler (FormResult (), Widget)
deleteForm extra = do
  (submitRes, submitView) <- mbootstrapSubmit (BootstrapSubmit MsgReallyDeleteSubmit "" [])
  let widget = $(widgetFile "delete-comment-form")
  return (submitRes, widget)

getDeleteCommentR :: CommentId -> Handler Html
getDeleteCommentR commentId = do
  comment <- runDB $ get404 commentId
  (formWidget, enctype) <- generateFormPost deleteForm
  defaultLayout $ do
    setTitleI MsgReallyDeleteCommentTitle
    $(widgetFile "delete-comment")

postDeleteCommentR :: CommentId -> Handler Html
postDeleteCommentR commentId = do
  ((res, _), _) <- runFormPost deleteForm
  comment <- runDB $ get404 commentId
  case res of
    FormSuccess _ -> do
      runDB $ deleteKey commentId
      setMessageI MsgCommentDeleted
      redirect $ EntryR (commentEntry comment)
    _ -> do
      setMessageI MsgCommentNotDeleted
      redirect $ EntryR (commentEntry comment)

