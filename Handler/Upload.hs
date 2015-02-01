module Handler.Upload
  ( getUploadR )
where

import Import
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

getUploadR :: UploadId -> Handler TypedContent
getUploadR uploadId = do
  upload <- runDB $ get404 uploadId
  let path = Text.unpack $ uploadPath upload
      mimeType = Text.encodeUtf8 $ uploadType upload
      content = ContentFile path Nothing
  return $ TypedContent mimeType content

