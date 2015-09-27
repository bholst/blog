module Handler.Uploads
  ( getUploadsR )
where

import Import

getUploadsR :: Handler Html
getUploadsR = do
  uploads <- runDB $ selectList [] [Desc UploadPosted]
  extra <- getExtra
  let pagename  = MsgUploadsTitle $ extraPagename extra
  defaultLayout $ do
    setTitleI pagename
    $(widgetFile "uploads")
