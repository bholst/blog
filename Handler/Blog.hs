{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Blog
  ( getBlogR
  , getBlogPageR
  , getCategoryR
  , getCategoryPageR
  , getAllEntries
  , getAllEntriesByCategory )
where

import Import
import Data.Maybe (fromMaybe)
import Handler.Entry (entryWidget)
import Yesod.RssFeed (rssLink)
import Yesod.AtomFeed (atomLink)

_NUMBER_OF_ENTRIES_PER_PAGE_ :: Int64
_NUMBER_OF_ENTRIES_PER_PAGE_ = 10

countPages :: Handler Int64 -> Handler Int
countPages cEntries = do
  entries <- cEntries
  case entries `divMod` _NUMBER_OF_ENTRIES_PER_PAGE_ of
    (n, 0) -> return (fromIntegral n)
    (n, _) -> return (fromIntegral (n + 1))

countEntries :: Handler Int64
countEntries =
  liftM (fromMaybe 0 . headMay . map unValue) $
  runDB $ select $ from $ \(_ :: SqlExpr (Entity Entry)) -> do
    return countRows

getEntries :: SqlQuery () -> Handler [(Entity Entry, Int, Maybe (Entity Upload))]
getEntries fil =
  liftM (map $ mapSnd3 unValue) $ runDB $ select expr
  where
  expr =
    from $ \(e `LeftOuterJoin` c `LeftOuterJoin` i) -> do
      on (e ^. EntryTitleImage ==. i ?. UploadId)
      on (c ?. CommentEntry ==. just (e ^. EntryId))
      orderBy [desc (e ^. EntryPosted)]
      groupBy (e ^. EntryId)
      groupBy (i ?. UploadId)
      fil
      return (e, count (c ?. CommentId), i)

getEntryPage :: Int -> Handler [(Entity Entry, Int, Maybe (Entity Upload))]
getEntryPage = getEntries . pageFilter

pageFilter :: Int -> SqlQuery ()
pageFilter page = do
  offset ((fromIntegral page) * _NUMBER_OF_ENTRIES_PER_PAGE_)
  limit _NUMBER_OF_ENTRIES_PER_PAGE_

getAllEntries :: Handler [(Entity Entry, Int, Maybe (Entity Upload))]
getAllEntries = getEntries (return ())

countCategoryEntries :: CategoryId -> Handler Int64
countCategoryEntries categoryId =
  liftM (fromMaybe 0 . headMay . map unValue) $
  runDB $ select $ from $ \(cat `InnerJoin` e) -> do
    where_ (cat ^. CategoryEntryCategory ==. val categoryId)
    on (cat ^. CategoryEntryEntry ==. e ^. EntryId)
    return (count (e ^. EntryId))

getEntriesByCategory :: SqlQuery () -> CategoryId -> Handler [(Entity Entry, Int, Maybe (Entity Upload))]
getEntriesByCategory fil categoryId =
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
      fil
      return (e, count (c ?. CommentId), i)

getAllEntriesByCategory :: CategoryId -> Handler [(Entity Entry, Int, Maybe (Entity Upload))]
getAllEntriesByCategory = getEntriesByCategory (return ())

getEntryPageByCategory :: Int -> CategoryId -> Handler [(Entity Entry, Int, Maybe (Entity Upload))]
getEntryPageByCategory = getEntriesByCategory . pageFilter

getBlogPageR :: Int -> Handler Html
getBlogPageR page = do
  entries <- getEntryPage page
  pageCount <- countPages countEntries
  let pages = take pageCount [0..]
      mkPagination = pageCount > 1
      routeCons = BlogPageR
  muser <- maybeAuth
  extra <- getExtra
  let pagename  = extraPagename extra
  defaultLayout $ do
    setTitle (toHtml pagename)
    rssLink  RssFeedR  pagename
    atomLink AtomFeedR pagename
    $(widgetFile "blog")

getBlogR :: Handler Html
getBlogR = redirect (BlogPageR 0)

getCategoryR :: CategoryId -> Handler Html
getCategoryR cat = redirect (CategoryPageR cat 0)

getCategoryPageR :: CategoryId -> Int -> Handler Html
getCategoryPageR categoryId page = do
  category <- runDB $ do
    get404 categoryId
  entries <- getEntryPageByCategory page categoryId
  pageCount <- countPages (countCategoryEntries categoryId)
  let pages = take pageCount [0..]
      mkPagination = pageCount > 1
      routeCons = CategoryPageR categoryId
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
