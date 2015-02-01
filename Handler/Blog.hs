module Handler.Blog
  ( getBlogR
  , getCategoryR
  , getEntries
  , getEntriesByCategory )
where

import Import
import qualified Data.Conduit.List as CL
import Data.Conduit
import Database.Persist.Sql (SqlPersistT)
import Handler.Entry (entryWidget, getUploads)
import Yesod.RssFeed (rssLink)
import Yesod.AtomFeed (atomLink)

entriesWithCommentsSource :: Source (SqlPersistT Handler) (Entity Entry, Int)
entriesWithCommentsSource =
  selectSource [] [Desc EntryPosted] $=
  countComments

getEntriesWithUploads :: Handler [(Entity Entry, Int, [Entity Upload])]
getEntriesWithUploads = runDB $
  entriesWithCommentsSource $=
  addUploads $$
  CL.consume

getEntries :: Handler [(Entity Entry, Int)]
getEntries = runDB $
  entriesWithCommentsSource $$
  CL.consume

getBlogR :: Handler Html
getBlogR = do
  entries <- getEntriesWithUploads
  muser <- maybeAuth
  extra <- getExtra
  let pagename  = extraPagename extra
      noEntries = MsgNoEntries
  defaultLayout $ do
    setTitle (toHtml pagename)
    rssLink  RssFeedR  pagename
    atomLink AtomFeedR pagename
    $(widgetFile "blog")

entriesWithCommentsByCategorySource :: CategoryId -> Source (SqlPersistT Handler) (Entity Entry, Int)
entriesWithCommentsByCategorySource categoryId = do
  categoryEntries <- lift $ selectList [CategoryEntryCategory ==. categoryId] []
  let entryIds = map (categoryEntryEntry . entityVal) categoryEntries
  selectSource [EntryId <-. entryIds] [Desc EntryPosted] $= countComments

getEntriesByCategory :: CategoryId -> Handler [(Entity Entry, Int)]
getEntriesByCategory categoryId = runDB $
  entriesWithCommentsByCategorySource categoryId $$ CL.consume

getEntriesByCategoryWithUploads :: CategoryId -> Handler [(Entity Entry, Int, [Entity Upload])]
getEntriesByCategoryWithUploads categoryId = runDB $
  entriesWithCommentsByCategorySource categoryId $=
  addUploads $$ CL.consume

getCategoryR :: CategoryId -> Handler Html
getCategoryR categoryId = do
  category <- runDB $ do
    get404 categoryId
  entries <- getEntriesByCategoryWithUploads categoryId
  muser <- maybeAuth
  extra <- getExtra
  let pagename = extraPagename extra
      name      = MsgCategoryTitle pagename $ categoryName category
      noEntries = (MsgNoCategoryEntries $ categoryName category)
  mr <- getMessageRender
  defaultLayout $ do
    setTitleI name
    rssLink  (CategoryRssFeedR  categoryId) (mr name)
    atomLink (CategoryAtomFeedR categoryId) (mr name)
    $(widgetFile "blog")

countComments :: (MonadIO m) =>
                 Conduit (Entity Entry) (SqlPersistT m) (Entity Entry, Int)
countComments = awaitForever (\entity -> do
  commentCount <- lift $ count [CommentEntry ==. (entityKey entity)]
  yield (entity, commentCount))

addUploads :: Conduit (Entity Entry, a) (SqlPersistT Handler) (Entity Entry, a, [Entity Upload])
addUploads = awaitForever $ \(entity, x) -> do
  uploads <- lift $ getUploads $ entityKey entity
  yield (entity, x, take 1 uploads)
