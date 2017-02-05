{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Lib where

import           Control.Arrow
import           Control.Lens
import           Control.Lens.Operators
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Aeson.TH
import           Data.Char                 (toLower)
import           Data.List
import           Data.Maybe
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           GHC.Generics
import           Safe
import           Test.WebDriver
import           Test.WebDriver.Class
import           Test.WebDriver.Commands

data ArticlePreview = ArticlePreview
  { _articlePreviewName          :: T.Text
  , _articlePreviewUrl           :: T.Text
  , _articlePreviewPerex         :: T.Text
  , _articlePreviewAuthorName    :: T.Text
  , _articlePreviewAuthorUrl     :: T.Text
  , _articlePreviewDate          :: T.Text
  , _articlePreviewCategoryName  :: T.Text
  , _articlePreviewCategoryUrl   :: T.Text
  , _articlePreviewCommentsCount :: Maybe Integer
  } deriving (Show, Eq, Generic)

makeFields ''ArticlePreview

deriveJSON
  defaultOptions {fieldLabelModifier = (_head %~ toLower) . drop 15}
  ''ArticlePreview

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

getTextFromMay :: Element -> T.Text -> WD (Maybe T.Text)
getTextFromMay e sel = do
  may <- findElemFromMay e sel
  mapM getText may

getTextFromDef :: Element -> T.Text -> T.Text -> WD T.Text
getTextFromDef e sel def = do
  textMay <- getTextFromMay e sel
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

elemToArticlePreview :: Element -> WD (Maybe ArticlePreview)
elemToArticlePreview e = do
  nameMay <- getTextFromMay e "h3.article__heading, h3.opener__heading"
  let name = fromMaybe "<Failed to get name>" nameMay
  url <- getUrlFromDef e "a.link-block" "<Failed to get url>"
  perex <- getTextFromDef e ".perex" "<Failed to get perex>"
  author <- getTextFromDef e ".impressum__author" "<Failed to get author>"
  authorUrl <- getUrlFromDef e ".impressum__author" "<Failed to get author url>"
  date <- getTextFromDef e ".impressum__date" "<Failed to get date>"
  category <- getTextFromDef e ".impressum__rubric" "<Failed to get category>"
  categoryUrl <- getUrlFromDef e ".impressum__rubric" "<Failed to get category url>"
  commentsCountStr <- getTextFromDef e ".comments__count" "<Failed to get comments count>"
  let commentsCount = readMay $ T.unpack commentsCountStr
  return $ if isNothing nameMay || T.null name then Nothing
           else Just ArticlePreview { _articlePreviewName = name
                                    , _articlePreviewUrl = url
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
  saveScreenshot "home.png"
  articleElems <- findElems $ ByCSS ".opener__body, .article.article--content"
  allArticlesMay <- mapM elemToArticlePreview articleElems
  let allArticles = catMaybes allArticlesMay
  let articles = filter (isFromZdrojak >>> not) allArticles
  return articles where
  isFromZdrojak :: ArticlePreview -> Bool
  isFromZdrojak a = (a^.url) & T.isInfixOf "zdrojak.cz"
