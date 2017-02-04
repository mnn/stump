{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans.Class
import           Data.Aeson                 (encode, FromJSON, ToJSON)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import           Data.List
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Lib
import           Test.WebDriver
import           Test.WebDriver.Session
import           Web.Scotty.Trans

encodeToString :: ToJSON a => a -> String
encodeToString = CL.unpack . encode

articlePreviews :: ActionT TL.Text WD ()
articlePreviews = do
  previews <- lift extractArticlePreviews
  let a = intercalate "\n" $ map encodeToString previews
  text $ "got this for you:\n" <> TL.pack a

routes :: ScottyT TL.Text WD ()
routes = do
  get "/article-previews" articlePreviews

startServer = startMyBrowser $ do
  sessionID <- getSession
  scottyT 3000 (runWD sessionID) routes
  stopMyBrowser

main :: IO ()
main = startServer
