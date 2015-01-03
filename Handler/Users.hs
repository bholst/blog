module Handler.Users
where

import Import

getUsersR :: Handler Html
getUsersR = do
  users <- runDB $ selectList [] [Asc UserIdent]
  defaultLayout $ do
    setTitleI MsgUsersTitle
    $(widgetFile "users")