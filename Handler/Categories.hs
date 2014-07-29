module Handler.Categories
where

import Import
import Handler.EditCategory

getCategoriesR :: Handler Html
getCategoriesR = do
  categories <- runDB $ selectList [] [Asc CategoryName]
  (newCategoryWidget, enctype) <- generateFormPost (categoryForm MsgNewCategorySubmit Nothing)
  defaultLayout $ do
    setTitleI MsgCategoriesTitle
    $(widgetFile "categories")

postCategoriesR :: Handler Html
postCategoriesR = do
  ((res, newCategoryWidget), enctype) <- runFormPost (categoryForm MsgNewCategorySubmit Nothing)
  case res of
    FormSuccess category -> do
      _ <- runDB $ insert category
      setMessageI $ MsgCategoryAdded $ categoryName category
      redirect CategoriesR
    _ -> do
      categories <- runDB $ selectList [] [Asc CategoryName]
      setMessageI $ MsgCategoryAddFailed
      defaultLayout $ do
        setTitleI MsgCategoriesTitle
        $(widgetFile "categories")