module Handler.DeleteCategory
  ( getDeleteCategoryR
  , postDeleteCategoryR )
where

import Import

deleteForm :: Category -> Html -> MForm Handler (FormResult (), Widget)
deleteForm category extra = do
  (submitRes, submitView) <- mbootstrapSubmit (BootstrapSubmit MsgReallyDeleteSubmit "" [])
  let widget = $(widgetFile "delete-category-form")
  return (submitRes, widget)

getDeleteCategoryR :: CategoryId -> Handler Html
getDeleteCategoryR categoryId = do
  category <- runDB $ get404 categoryId
  (formWidget, enctype) <- generateFormPost (deleteForm category)
  defaultLayout $ do
    setTitleI $ MsgReallyDeleteCategoryTitle $ categoryName category
    $(widgetFile "delete-category")

postDeleteCategoryR :: CategoryId -> Handler Html
postDeleteCategoryR categoryId = do
  category <- runDB $ get404 categoryId
  ((res, _), _) <- runFormPost (deleteForm category)
  case res of
    FormSuccess _ -> do
      runDB $ deleteCascade categoryId
      setMessageI $ MsgCategoryDeleted $ categoryName category
      redirect CategoriesR
    _ -> do
      setMessageI $ MsgCategoryNotDeleted $ categoryName category
      redirect CategoriesR