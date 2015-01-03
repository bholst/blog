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
  let adminFieldSettingsBase = bfs MsgIsAdmin :: FieldSettings App
      adminFieldSettings =
        case mauth of
          Just (Entity authId _) | authId == userId ->
            adminFieldSettingsBase
            { fsAttrs = ("disabled", "") : (fsAttrs adminFieldSettingsBase) }
          _ ->
            adminFieldSettingsBase
  (adminRes, adminView) <- mreq checkBoxField adminFieldSettings (Just $ userAdmin user)
  (submitRes, submitView) <- mbootstrapSubmit (BootstrapSubmit msg "" [])
  let widget = $(widgetFile "edit-user-form")
      userRes = mkUser <$> adminRes
  return (userRes, widget)
  where
    mkUser :: Bool -> User
    mkUser admin = user { userAdmin = admin }

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