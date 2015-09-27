module Handler.Blog
  ( getBlogR
  , getCategoryR
  , getEntries
  , getEntriesByCategory )
where

import Import
import Handler.Entry (entryWidget)
import Yesod.RssFeed (rssLink)
import Yesod.AtomFeed (atomLink)

getEntries :: Handler [(Entity Entry, Int, Maybe (Entity Upload))]
getEntries  =
  liftM (map $ mapSnd3 unValue) $ runDB $ select expr
  where
  expr =
    from $ \(e `LeftOuterJoin` c `LeftOuterJoin` i) -> do
      on (e ^. EntryTitleImage ==. i ?. UploadId)
      on (c ?. CommentEntry ==. just (e ^. EntryId))
      orderBy [desc (e ^. EntryPosted)]
      groupBy (e ^. EntryId)
      groupBy (i ?. UploadId)
      return (e, count (c ?. CommentId), i)

getEntriesByCategory :: CategoryId -> Handler [(Entity Entry, Int, Maybe (Entity Upload))]
getEntriesByCategory categoryId =
  liftM (map $ mapSnd3 unValue) $ runDB $ select expr
  where
  expr =
    from $ \(cat `InnerJoin` e `LeftOuterJoin` c `LeftOuterJoin` i) -> do
      where_ (cat ^. CategoryEntryCategory ==. val categoryId)
      on (e ^. EntryTitleImage ==. i ?. UploadId)
      on (just (e ^. EntryId) ==. c ?. CommentEntry)
      on (cat ^. CategoryEntryEntry ==. e ^. EntryId)
      orderBy [desc (e ^. EntryPosted)]
      groupBy (e ^. EntryId)
      groupBy (i ?. UploadId)
      return (e, count (c ?. CommentId), i)

getBlogR :: Handler Html
getBlogR = do
  entries <- getEntries
  muser <- maybeAuth
  extra <- getExtra
  let pagename  = extraPagename extra
  defaultLayout $ do
    setTitle (toHtml pagename)
    rssLink  RssFeedR  pagename
    atomLink AtomFeedR pagename
    $(widgetFile "blog")

getCategoryR :: CategoryId -> Handler Html
getCategoryR categoryId = do
  category <- runDB $ do
    get404 categoryId
  entries <- getEntriesByCategory categoryId
  muser <- maybeAuth
  extra <- getExtra
  let pagename = extraPagename extra
      name      = MsgCategoryTitle pagename $ categoryName category
  mr <- getMessageRender
  defaultLayout $ do
    setTitleI name
    rssLink  (CategoryRssFeedR  categoryId) (mr name)
    atomLink (CategoryAtomFeedR categoryId) (mr name)
    $(widgetFile "blog")
