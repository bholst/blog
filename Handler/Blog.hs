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
import Yesod.RssFeed (rssLink)
import Yesod.AtomFeed (atomLink)

getEntries :: Handler [(Entity Entry, Int)]
getEntries = runDB $
  selectSource [] [Desc EntryPosted] =$ countComments $$ CL.consume

getBlogR :: Handler Html
getBlogR = do
  entries <- getEntries
  muser <- maybeAuth
  extra <- getExtra
  let pagename  = extraPagename extra
      noEntries = MsgNoEntries
  defaultLayout $ do
    setTitle (toHtml pagename)
    rssLink  RssFeedR  pagename
    atomLink AtomFeedR pagename
    $(widgetFile "blog")

getEntriesByCategory :: CategoryId -> Handler [(Entity Entry, Int)]
getEntriesByCategory categoryId = runDB $ do
  categoryEntries <- selectList [CategoryEntryCategory ==. categoryId] []
  let entryIds = map (categoryEntryEntry . entityVal) categoryEntries
  selectSource [EntryId <-. entryIds] [Desc EntryPosted] =$ countComments $$ CL.consume

getCategoryR :: CategoryId -> Handler Html
getCategoryR categoryId = do
  category <- runDB $ do
    get404 categoryId
  entries <- getEntriesByCategory categoryId
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
