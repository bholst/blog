module Handler.NewUpload
  ( getNewUploadR
  , postNewUploadR )
where

import Import
import Data.Conduit
import Data.Conduit.Combinators (sinkHandle)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.IO.Temp (openTempFile)

fileUploadForm :: Maybe (FileInfo, Text) -> Form (FileInfo, Text)
fileUploadForm x = renderDivs $ (,)
  <$> areq fileField (bfs MsgUploadFileField) (fst <$> x)
  <*> areq textField (bfs MsgUploadFileDescription) (snd <$> x)
  <* bootstrapSubmit (BootstrapSubmit MsgUploadFileSubmit "" [])

getNewUploadR :: Handler Html
getNewUploadR = do
  (newUploadWidget, enctype) <- generateFormPost (fileUploadForm Nothing)
  defaultLayout $ do
    setTitleI MsgNewUploadTitle
    $(widgetFile "newupload")

postNewUploadR :: Handler Html
postNewUploadR = do
  ((res, newUploadWidget), enctype) <- runFormPost (fileUploadForm Nothing)
  case res of
    FormSuccess (fi, description) -> do
      let contentType = fileContentType fi
          uploadDir = "uploads"
      liftIO $ createDirectoryIfMissing True uploadDir
      (filePath, handle) <- liftIO $ openTempFile uploadDir "upload"
      liftIO $ putStrLn $ show $ fileContentType fi
      let source = fileSource fi
      source $$ sinkHandle handle
      now <- lift getCurrentTime
      completePath <- liftIO $ canonicalizePath filePath
      _ <- runDB $ insert (Upload description (Text.pack completePath) now contentType)
      setMessageI MsgUploadSaved
      redirect UploadsR
    _ -> do
      setMessageI MsgFileUploadFailed
      defaultLayout $ do
        setTitleI MsgNewUploadTitle
        $(widgetFile "newupload")