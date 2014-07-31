module Handler.Feed where

import Import
import Yesod.Feed
import Yesod.RssFeed (rssFeed, RepRss)
import Yesod.AtomFeed (atomFeed, RepAtom)
import Data.Time.Clock
import Handler.Blog (getEntries, getEntriesByCategory)

getRssFeedR :: Handler RepRss
getRssFeedR = (map fst) <$> getEntries >>= getFeed RssFeedR >>= rssFeed

getAtomFeedR :: Handler RepAtom
getAtomFeedR = (map fst) <$> getEntries >>= getFeed AtomFeedR >>= atomFeed

getCategoryRssFeedR :: CategoryId -> Handler RepRss
getCategoryRssFeedR categoryId =
  (map fst) <$> (getEntriesByCategory categoryId)
  >>= getFeed RssFeedR >>= rssFeed

getCategoryAtomFeedR :: CategoryId -> Handler RepAtom
getCategoryAtomFeedR categoryId =
  (map fst) <$> (getEntriesByCategory categoryId)
  >>= getFeed AtomFeedR >>= atomFeed

getFeed :: Route (HandlerSite Handler) -> [Entity Entry] -> Handler (Feed (Route (HandlerSite Handler)))
getFeed feed blogEntries = do
    let entries = map entry2FeedEntry blogEntries
    extra <- getExtra
    let title  = extraPagename extra
        author = extraAuthor   extra
        language = "en"
    updated <- case entries of
                 [] -> liftIO $ getCurrentTime
                 _  -> return $ foldl1 max (map (entryUpdated . entityVal) blogEntries)
    return $ Feed title feed BlogR author (toHtml ("" :: Text)) language updated entries
 where
  entry2FeedEntry (Entity entryId entry) =
    FeedEntry (EntryR entryId) (entryPosted entry) (entryTitle entry) (toHtml $ entryContent entry)
