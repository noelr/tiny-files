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

staticFile = (</>) "static"
downloadFile = (</>) "download"

go = scotty 4001 $ do
  middleware logStdoutDev
  get "/" $ file $ staticFile "index.html"
  get "/files" $ do
    f <- liftIO $ listDirectory "download"
    let fs = fmap UserFile f
    json fs
  get "/main.js" $ file $ staticFile "main.js"
  get "/download/:file" $ do
    name <- param "file"
    setHeader "content-disposition" "attachment"
    file $ downloadFile name
  delete "/delete/:file" $ do
    name <- param "file"
    liftIO $ removeFile $ downloadFile name
    text "OK"
  post "/upload" $ do
      fs <- files
      let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]
      -- write the files to disk
      liftIO $ sequence_ [ B.writeFile (downloadFile fn) fc | (_,fn,fc) <- fs' ]
      text "OK"
