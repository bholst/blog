module Handler.Uploads
  ( getUploadsR )
where

import Import
import Yesod.Core (renderRoute)
import qualified Data.Text as Text

imageTemplate :: UploadId -> Text
imageTemplate uploadId =
  "![Alt text](/" `Text.append` (Text.intercalate "/" (fst (renderRoute (UploadR uploadId)))) `Text.append` ")"

getUploadsR :: Handler Html
getUploadsR = do
  uploads <- runDB $ selectList [] [Desc UploadPosted]
  muser <- maybeAuth
  extra <- getExtra
  let pagename  = MsgUploadsTitle $ extraPagename extra
      noUploads = MsgNoUploads
  defaultLayout $ do
    setTitleI pagename
    $(widgetFile "uploads")
