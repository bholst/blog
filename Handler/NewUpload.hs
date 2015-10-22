module Handler.NewUpload
  ( getNewUploadR
  , postNewUploadR )
where

import Import
import Control.Exception.Base (bracket)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit
import Data.Conduit.Combinators (sinkHandle)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import qualified Graphics.GD as GD
import System.Directory (canonicalizePath, createDirectoryIfMissing, removeFile)
import System.IO (hClose)
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
      filePath <-
        liftIO $ bracket (openTempFile uploadDir "upload") (\(_, h) -> hClose h) $ \(fp, h) -> do
          let source = fileSource fi
          runResourceT $ source $$ sinkHandle h
          return fp
      extra <- getExtra
      uploadError <-
        liftIO $
        case forceImageRatio extra of
          Nothing -> return Nothing
          Just forceRatio ->
            case Text.unpack contentType of
              'i':'m':'a':'g':'e':'/':fmt ->
                let mLoad =
                      case fmt of
                        "jpeg" -> Just GD.loadJpegFile
                        "png" -> Just GD.loadPngFile
                        "gif" -> Just GD.loadGifFile
                        _ -> Nothing
                in case mLoad of
                     Nothing -> return $ Just (MsgUnsupportedImageType contentType)
                     Just load ->
                       GD.withImage (load filePath) $ \image -> do
                         (w, h) <- GD.imageSize image
                         case (round ((fromIntegral h) * forceRatio)) - w of
                           0 -> return Nothing
                           _ ->
                             return $
                             Just (MsgWrongAspectRatio forceRatio (fromIntegral w / fromIntegral h))
              _ -> return Nothing
      case uploadError of
        Nothing -> addFileToDatabase filePath description contentType
        Just err -> do
          liftIO $ removeFile filePath
          setMessageI err
          defaultLayout $ do
            setTitleI MsgNewUploadTitle
            $(widgetFile "newupload")
    _ -> do
      setMessageI MsgFileUploadFailed
      defaultLayout $ do
        setTitleI MsgNewUploadTitle
        $(widgetFile "newupload")
    where
      addFileToDatabase :: String -> Text -> Text -> Handler Html
      addFileToDatabase filePath description contentType = do
        now <- lift getCurrentTime
        completePath <- liftIO $ canonicalizePath filePath
        _ <- runDB $ insert (Upload description (Text.pack completePath) now contentType)
        setMessageI MsgUploadSaved
        redirect UploadsR
