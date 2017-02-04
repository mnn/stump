{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Lib where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Maybe
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           Safe
import           Test.WebDriver
import           Test.WebDriver.Class
import           Test.WebDriver.Commands

data ArticlePreview = ArticlePreview
  { _articlePreviewName          :: T.Text
  , _articlePreviewPreview       :: T.Text
  , _articlePreviewAuthorName    :: T.Text
  , _articlePreviewAuthorUrl     :: T.Text
  , _articlePreviewDate          :: T.Text
  , _articlePreviewCategoryName  :: T.Text
  , _articlePreviewCategoryUrl   :: T.Text
  , _articlePreviewCommentsCount :: Integer
  } deriving (Show, Eq)

firefoxConfig :: WDConfig
firefoxConfig = defaultConfig

startMyBrowser :: WD a -> IO a
startMyBrowser = runSession firefoxConfig

stopMyBrowser :: WebDriver wd => wd ()
stopMyBrowser = closeSession

urlPrefix = "http://www.root.cz"
homeUrl = urlPrefix

findElemFromMay :: Element -> T.Text -> WD (Maybe Element)
findElemFromMay e sel = do
  elems <- findElemsFrom e $ ByCSS sel
  return $ headMay elems

getTextFromDef :: Element -> T.Text -> T.Text -> WD T.Text
getTextFromDef e sel def = do
  may <- findElemFromMay e sel
  textMay <- mapM getText may
  return $ fromMaybe def textMay

elemToArticlePreview :: Element -> WD ArticlePreview
elemToArticlePreview e = do
  name <- getTextFromDef e ".article__heading.article__heading--content" "<Failed to get name>"
  -- TODO: extract more article fields
  return ArticlePreview {
    _articlePreviewName = name
  , _articlePreviewPreview = ""
  , _articlePreviewAuthorName = ""
  , _articlePreviewAuthorUrl = ""
  , _articlePreviewDate = ""
  , _articlePreviewCategoryName = ""
  , _articlePreviewCategoryUrl = ""
  , _articlePreviewCommentsCount= -1
  }

extractArticlePreviews :: WD [ArticlePreview]
extractArticlePreviews = do
  openPage homeUrl
  -- TODO: featured article
  articleElems <- findElems $ ByCSS ".article.article--content"
  articles <- mapM elemToArticlePreview articleElems
  return articles
