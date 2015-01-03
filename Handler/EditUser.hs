module Handler.EditUser where

import Import

userForm :: RenderMessage App msg =>
            msg
         -> UserId
         -> User
         -> Html
         -> MForm Handler (FormResult User, Widget)
userForm msg userId user extra = do
  mauth <- lift maybeAuth
  let ownPage =
        case mauth of
          Just (Entity authId _) | authId == userId -> True
          _ -> False
      adminFieldSettingsBase = bfs MsgIsAdmin :: FieldSettings App
      adminFieldSettings =
        if ownPage
          then
            adminFieldSettingsBase
            { fsAttrs = ("disabled", "") : (fsAttrs adminFieldSettingsBase) }
          else
            adminFieldSettingsBase
  (adminRes, adminView) <- mopt checkBoxField adminFieldSettings (Just $ Just $ userAdmin user)
  (submitRes, submitView) <- mbootstrapSubmit (BootstrapSubmit msg "" [])
  liftIO $ putStrLn $ "Admin: " ++ show adminRes
  let widget = $(widgetFile "edit-user-form")
      adminResCorrect =
        if ownPage
          then
            pure $ Just $ userAdmin user
          else
            adminRes
      userRes = mkUser <$> adminResCorrect
  return (userRes, widget)
  where
    mkUser :: Maybe Bool -> User
    mkUser Nothing =
      user
    mkUser (Just admin) =
      user { userAdmin = admin }

getEditUserR :: UserId -> Handler Html
getEditUserR userId = do
  user <- runDB $ get404 userId
  (formWidget, enctype) <- generateFormPost
    (userForm MsgEditUserSubmit userId user)
  defaultLayout $ do
    setTitleI $ MsgEditUserTitle $ userIdent user
    $(widgetFile "edit-user")

postEditUserR :: UserId -> Handler Html
postEditUserR userId = do
  oldUser <- runDB $ get404 userId
  ((res, formWidget), enctype) <-
    runFormPost (userForm MsgEditUserSubmit userId oldUser)
  case res of
    FormSuccess user -> do
      runDB $ replace userId user
      setMessageI $ MsgUserEdited $ userIdent user
      redirect UsersR
    _ -> defaultLayout $ do
      let user = oldUser
      setTitleI MsgPleaseCorrectUser
      $(widgetFile "edit-user")