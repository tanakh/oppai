{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}

import Control.Applicative
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as C
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Conduit
import System.Directory
import System.FilePath
import Text.Shakespeare.Text

baseUrl, saveDir, bingAppId :: String
baseUrl = "http://api.bing.net/json.aspx"
saveDir = "/tmp/oppai"
bingAppId = "3ED86F72427CA7588F73A9CA5D1FF69E3EB24F03"

threadNum = 10

main :: IO ()
main = do
  createDirectoryIfMissing True saveDir
  urls <- newMVar []
  withManager $ \mng -> lift $ forM_ [1 .. threadNum] $ \tid -> forkIO $ do
    forever $ ign_ $ takeMVar urls >>= getImage tid mng
  withManager $ \mng -> lift $ forM_ [0..] $ ign_ . getUrl urls mng

getImage tid mng url = do
  T.putStrLn $ [st|[#{show tid}]: Downloading #{url} ...|]
  C.runResourceT $ do
    req <- parseUrl url
    Response {..} <- http req mng
    responseBody C.$$ C.sinkFile (saveDir </> takeFileName url)

newtype Urls = Urls { unUrls :: [String] }
instance FromJSON Urls where
  parseJSON (Object v) = do
    rs <- (.: "SearchResponse") >=> (.: "Image") >=> (.: "Results") $ v
    Urls <$> mapM (\(Object v) -> v .: "MediaUrl") rs
  parseJSON _ = mzero

getUrl urls mng page = do
  let ofs = page * 50
  req <- parseUrl $ T.unpack [st|#{baseUrl}?AppId=#{bingAppId}&Version=2.2&Market=ja-JP&Sources=Image&Image.Count=50&Image.Offset=#{show ofs}&Adult=off&Query=おっぱい|]
  Response {..} <- C.runResourceT $ httpLbs req mng
  let us = maybe [] unUrls $ decode' responseBody
  mapM_ (putMVar urls) us

ign_ = E.handle (\(E.SomeException e)-> print e)
