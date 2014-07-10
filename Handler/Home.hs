module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR =
    defaultLayout $ do
        aDomId <- newIdent
        setTitleI MsgHomepageTitle
        $(widgetFile "homepage")

