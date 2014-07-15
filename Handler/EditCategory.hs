module Handler.EditCategory where

import Import

categoryForm :: RenderMessage App msg => msg -> Maybe Category -> Form Category
categoryForm msg category = renderDivs $ Category
    <$> areq textField (bfs MsgNewCategoryName) (categoryName <$> category)
    <*  bootstrapSubmit (BootstrapSubmit msg "" [])

getEditCategoryR :: CategoryId -> Handler Html
getEditCategoryR categoryId = do
  category <- runDB $ get404 categoryId
  (formWidget, enctype) <- generateFormPost
    (categoryForm MsgEditCategorySubmit $ Just category)
  defaultLayout $ do
    setTitleI $ MsgEditCategoryTitle $ categoryName category
    $(widgetFile "edit-category")

postEditCategoryR :: CategoryId -> Handler Html
postEditCategoryR categoryId = do
  oldCategory <- runDB $ get404 categoryId
  ((res, formWidget), enctype) <-
    runFormPost (categoryForm MsgEditCategorySubmit $ Just oldCategory)
  case res of
    FormSuccess category -> do
      runDB $ replace categoryId category
      setMessageI $ MsgCategoryEdited $ categoryName category
      redirect CategoriesR
    _ -> defaultLayout $ do
      setTitleI MsgPleaseCorrectCategory
      $(widgetFile "edit-category")