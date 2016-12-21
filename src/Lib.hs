{-# LANGUAGE OverloadedStrings #-}

module Lib (go) where

import Web.Scotty

import Control.Monad.IO.Class

import Network.Wai.Parse

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import System.FilePath ((</>))

go = scotty 3000 $ do
  get "/" $ file "./static/index.html"
  get "/download" $ do
    setHeader "content-disposition" "attachment"
    file "./static/file"
  post "/upload" $ do
      fs <- files
      let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]
      -- write the files to disk, so they will be served by the static middleware
      liftIO $ sequence_ [ B.writeFile ("static/file") fc | (_,_,fc) <- fs' ]
      text "OK"
