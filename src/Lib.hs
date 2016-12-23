{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib (go) where

import Web.Scotty

import Control.Monad.IO.Class

import Network.Wai.Parse
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import System.FilePath ((</>))
import System.Directory

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data UserFile = UserFile { name :: FilePath } deriving (Show, Generic)
instance ToJSON UserFile

go = scotty 4001 $ do
  middleware logStdoutDev
  get "/" $ file "static/index.html"
  get "/files" $ do
    f <- liftIO $ listDirectory "download"
    let fs = fmap UserFile f
    json fs
  get "/main.js" $ file "static/main.js"
  get "/download/:file" $ do
    name <- param "file"
    setHeader "content-disposition" "attachment"
    file $ "download/" ++ name
  post "/upload" $ do
      fs <- files
      let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]
      -- write the files to disk
      liftIO $ sequence_ [ B.writeFile ("download/" ++ fn) fc | (_,fn,fc) <- fs' ]
      text "OK"
