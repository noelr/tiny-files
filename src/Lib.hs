{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib (go) where

import Servant
import Network.Wai.Handler.Warp
import Control.Monad.Except
import Network.Wai.Middleware.RequestLogger
import Data.Aeson
import GHC.Generics

go :: IO ()
go = run 8080 $ logStdoutDev app

data UploadFile = UploadFile {file :: String} deriving (Show, Generic)
instance ToJSON UploadFile
instance FromJSON UploadFile

type FilesApi = "download" :> Get '[PlainText] String
  :<|> "upload" :> ReqBody '[JSON] UploadFile :> Post '[PlainText] String
  :<|> Raw

upload :: UploadFile -> ExceptT ServantErr IO String
upload uf = do
  liftIO $ writeFile "files/file" $ file uf
  return "ok"

download :: ExceptT ServantErr IO String
download = liftIO $ readFile "files/file"

server :: Server FilesApi
server = download
  :<|> upload
  :<|> serveDirectory "./static"

api :: Proxy FilesApi
api = Proxy

app :: Application
app = serve api server
