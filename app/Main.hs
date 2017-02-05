{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow
import           Control.Lens.Operators
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Aeson                 (FromJSON, ToJSON, encode)
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Text
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
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE

wrapString :: String -> String -> String -> String
wrapString l r x = l ++ x ++ r

wrapLazyText :: TL.Text -> TL.Text -> TL.Text -> TL.Text
wrapLazyText l r x = l `TL.append` x `TL.append` r

encodePrettyToLazyText :: ToJSON a => a -> TL.Text
encodePrettyToLazyText = encodePretty >>> TLE.decodeUtf8

help :: ActionT TL.Text WD ()
help = html "<html><body><a href='articles/previews'>article previews</a></body></html>"

articlePreviews :: ActionT TL.Text WD ()
articlePreviews = do
  previews <- lift extractArticlePreviews
  previews & encodePrettyToLazyText & text

routes :: ScottyT TL.Text WD ()
routes = do
  get "/" help
  get "/articles/previews" articlePreviews

startServer = startMyBrowser $ do
  sessionID <- getSession
  scottyT 3000 (runWD sessionID) routes
  stopMyBrowser

main :: IO ()
main = startServer
