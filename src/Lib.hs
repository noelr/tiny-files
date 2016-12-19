{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module Lib where

import Network.Wai.Middleware.RequestLogger
import Control.Monad.Trans.Resource
import Control.Monad.Except

import Data.Monoid
import Data.Text (Text)
import qualified Data.ByteString.Lazy as LS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TIO

import Network.Wai.Parse
import Network.Wai.Handler.Warp hiding (FileInfo)
import Servant
import Servant.Server.Internal
import System.Directory
import System.FilePath

data Mem
data Tmp

class KnownBackend b where
  type Storage b :: * -- associated type family

  withBackend :: Proxy b -> (BackEnd (Storage b) -> IO r) -> IO r

instance KnownBackend Mem where
  type Storage Mem = LS.ByteString

  withBackend Proxy f = f lbsBackEnd

instance KnownBackend Tmp where
  type Storage Tmp = FilePath

  withBackend Proxy f = runResourceT . withInternalState $ \s ->
    f (tempFileBackEnd s)

data Files b

type MultiPartData b = ([Param], [File (Storage b)])

instance (KnownBackend b, HasServer sublayout config)
      => HasServer (Files b :> sublayout) config where

  type ServerT (Files b :> sublayout) m =
    IO (MultiPartData b) -> ServerT sublayout m

  route Proxy config subserver =
    route psub config (addBodyCheck subserver check)
    where
      psub  = Proxy :: Proxy sublayout
      pbak  = Proxy :: Proxy b
      check = withRequest $ \request -> return $ withBackend pbak $
        \backend -> parseRequestBody backend request

type API = "upload" :> Files Mem :> Post '[PlainText] Text
      :<|> Raw

-- | Handle the uploaded image and video files.
upload :: IO (MultiPartData Mem) -> ExceptT ServantErr IO Text
upload multipart = do
  liftIO $ putStrLn "handling file upload..."
  (params, files) <- liftIO multipart

  liftIO $ mapM_ wrFile files
  return "OK"
  where
    wrFile (_, FileInfo{..}) = LS.writeFile newFileName fileContent
      where newFileName = (Text.unpack . Text.decodeUtf8) "static/download"


server :: Server API
server = upload
  :<|> serveDirectory "./static"

go :: IO ()
go = run 8888 $ logStdoutDev (serve (Proxy :: Proxy API) server)
