module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR =
  redirect BlogR

