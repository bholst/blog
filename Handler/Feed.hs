module Handler.Feed where

import Import
import Yesod.Feed
import Yesod.RssFeed (rssFeed, RepRss)
import Yesod.AtomFeed (atomFeed, RepAtom)
import Data.Time.Clock

getRssFeedR :: Handler RepRss
getRssFeedR = getFeed >>= rssFeed

getAtomFeedR :: Handler RepAtom
getAtomFeedR = getFeed >>= atomFeed

getFeed :: Handler (Feed (Route (HandlerSite Handler)))
getFeed = do
    entries <- runDB $ do
      entries <- selectList [] [Desc EntryPosted]
      return $ map entry2FeedEntry entries
    extra <- getExtra
    let title  = extraPagename extra
        author = extraAuthor   extra
        language = "en"
    updated <- case entries of
                 [] -> liftIO $ getCurrentTime
                 _  -> return $ foldl1 max (map feedEntryUpdated entries)
    return $ Feed title RssFeedR BlogR author (toHtml ("" :: Text)) language updated entries
 where
  entry2FeedEntry (Entity entryId entry) =
    FeedEntry (EntryR entryId) (entryPosted entry) (entryTitle entry) (entryContent entry)
