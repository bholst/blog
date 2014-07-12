module Handler.Blog where

import Import
import Data.Conduit.List as CL
import Data.Conduit

getBlogR :: Handler Html
getBlogR = do
  muser <- maybeAuth
  entries <- runDB $
    selectSource [] [Desc EntryPosted] =$ countComments $$ CL.consume
  defaultLayout $ do
    setTitleI MsgBlogTitle
    $(widgetFile "blog")
  where
    countComments = awaitForever (\entity -> do
      commentCount <- count [CommentEntry ==. (entityKey entity)]
      yield (entity, commentCount))
