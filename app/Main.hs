{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans.Class
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import           Lib
import           Test.WebDriver
import           Test.WebDriver.Session
import           Web.Scotty.Trans

articlePreviews :: ActionT TL.Text WD ()
articlePreviews = do
  a <- lift extractArticlePreviews
  text $ "got this for you:\n" <> TL.pack (show a)

routes :: ScottyT TL.Text WD ()
routes = do
  get "/article-previews" articlePreviews

startServer = startMyBrowser $ do
  sessionID <- getSession
  scottyT 3000 (runWD sessionID) routes
  stopMyBrowser

main :: IO ()
main = startServer
