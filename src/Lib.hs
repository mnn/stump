{-# LANGUAGE DeriveGeneric          #-}
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
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Maybe
import           Data.Monoid               ((<>))
import           Data.Text
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           GHC.Generics
import           Safe
import           Test.WebDriver
import           Test.WebDriver.Class
import           Test.WebDriver.Commands

data ArticlePreview = ArticlePreview
  { _articlePreviewName          :: T.Text
  , _articlePreviewPerex         :: T.Text
  , _articlePreviewAuthorName    :: T.Text
  , _articlePreviewAuthorUrl     :: T.Text
  , _articlePreviewDate          :: T.Text
  , _articlePreviewCategoryName  :: T.Text
  , _articlePreviewCategoryUrl   :: T.Text
  , _articlePreviewCommentsCount :: Maybe Integer
  } deriving (Show, Eq, Generic)

makeFields ''ArticlePreview

instance ToJSON ArticlePreview
instance FromJSON ArticlePreview

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

getUrl :: T.Text -> Element -> WD T.Text
getUrl def e = do
  may <- attr e "href"
  return $ fromMaybe def may

getUrlFromDef :: Element -> T.Text -> T.Text -> WD T.Text
getUrlFromDef e sel def = do
  may <- findElemFromMay e sel
  textMay <- mapM (getUrl def) may
  return $ fromMaybe def textMay

elemToArticlePreview :: Element -> WD ArticlePreview
elemToArticlePreview e = do
  name <- getTextFromDef e ".article__heading" "<Failed to get name>"
  perex <- getTextFromDef e ".perex" "<Failed to get perex>"
  author <- getTextFromDef e ".impressum__author" "<Failed to get author>"
  authorUrl <- getUrlFromDef e ".impressum__author" "<Failed to get author url>"
  date <- getTextFromDef e ".impressum__date" "<Failed to get date>"
  category <- getTextFromDef e ".impressum__rubric" "<Failed to get category>"
  categoryUrl <- getUrlFromDef e ".impressum__rubric" "<Failed to get category>"
  commentsCountStr <- getTextFromDef e ".comments__count" "<Failed to get comments count>"
  let commentsCount = readMay $ T.unpack commentsCountStr
  return ArticlePreview {
    _articlePreviewName = name
  , _articlePreviewPerex = perex
  , _articlePreviewAuthorName = author
  , _articlePreviewAuthorUrl = authorUrl
  , _articlePreviewDate = date
  , _articlePreviewCategoryName = category
  , _articlePreviewCategoryUrl = categoryUrl
  , _articlePreviewCommentsCount = commentsCount
  }

extractArticlePreviews :: WD [ArticlePreview]
extractArticlePreviews = do
  openPage homeUrl
  -- TODO: featured article
  articleElems <- findElems $ ByCSS ".article.article--content"
  articles <- mapM elemToArticlePreview articleElems
  return articles
