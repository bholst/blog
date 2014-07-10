module Handler.Blog where

import Import
import Data.Time.Clock
import Yesod.Form.Nic

entryForm :: Form Entry
entryForm = renderDivs $ Entry
    <$> areq textField (bfs MsgNewEntryTitle) Nothing
    <*> lift (liftIO getCurrentTime)
    <*> areq nicHtmlField (bfs MsgNewEntryContent) Nothing
    <*  bootstrapSubmit (BootstrapSubmit MsgNewEntry "" [])

getBlogR :: Handler Html
getBlogR = do
    muser <- maybeAuth
    entries <- runDB $ selectList [] [Desc EntryPosted]
    (entryWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        setTitleI MsgBlogArchiveTitle
        $(widgetFile "blog")

postBlogR :: Handler Html
postBlogR = do
    ((res, entryWidget), enctype) <- runFormPost entryForm
    case res of
        FormSuccess entry -> do
            entryId <- runDB $ insert entry
            setMessageI $ MsgEntryCreated $ entryTitle entry
            redirect $ EntryR entryId
        _ -> defaultLayout $ do
            setTitleI MsgPleaseCorrectEntry
            $(widgetFile "newentry")
