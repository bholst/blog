module Handler.Blog where

import Import
import Data.Conduit.List as CL
import Data.Conduit
import Yesod.RssFeed (rssLink)
import Yesod.AtomFeed (atomLink)

getBlogR :: Handler Html
getBlogR = do
  muser <- maybeAuth
  entries <- runDB $
    selectSource [] [Desc EntryPosted] =$ countComments $$ CL.consume
  mr <- getMessageRender
  defaultLayout $ do
    setTitleI MsgBlogTitle
    rssLink RssFeedR $ mr MsgHomepageTitle
    atomLink AtomFeedR $ mr MsgHomepageTitle
    $(widgetFile "blog")
  where
    countComments = awaitForever (\entity -> do
      commentCount <- count [CommentEntry ==. (entityKey entity)]
      yield (entity, commentCount))
