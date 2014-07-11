module Handler.Blog where

import Import

getBlogR :: Handler Html
getBlogR = do
    muser <- maybeAuth
    entries <- runDB $ selectList [] [Desc EntryPosted]
    defaultLayout $ do
        setTitleI MsgBlogArchiveTitle
        $(widgetFile "blog")

