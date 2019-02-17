{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
module Main where

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Data.Text.Lazy hiding (map)
import qualified Data.Text as ST
import Data.Text.Lazy.Encoding
import qualified Data.Text.Encoding as STE
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Lazy as LBS
import System.Directory

main :: IO ()
main = do
  let promises = map (\p -> defaultPushPromise
                       { promisedPath = "/" <> LBS.toStrict p
                       , promisedFile = "./" <> unpack (decodeUtf8 p)
                       , promisedResponseHeaders =
                         [ (CI.mk "content-type", "image/jpeg")
                         ]
                       }
                     ) images
  let cssPromise = defaultPushPromise
                   { promisedPath = "/style.css"
                   , promisedFile = "./style.css"
                   , promisedResponseHeaders =
                       [ (CI.mk "content-type", "text/css")
                       ]
                   }
  runTLS
    (tlsSettings "localhost.pem" "localhost-key.pem")
    (defaultSettings)
    (app (cssPromise : promises))

images :: [LBS.ByteString]
images =  map (\c -> encodeUtf8 $ pack ("img/" <> [c] <> ".jpg")) ['a'..'j']

app :: [PushPromise] -> Application
app promises req respond = do
  case pathInfo req of
    [] -> do
      getHTTP2Data req >>= \case
        Just _ -> return ()
        Nothing -> do
          setHTTP2Data req . Just $ defaultHTTP2Data
            { http2dataPushPromise = promises
            }
      respond $ responseLBS status200
        [ (CI.mk "content-type", "text/html")
        ] $
        "<html><head><title>http2warp</title><link rel=\"stylesheet\" href=\"style.css\"></head><body>" <>
        mconcat (map (\p -> "<img src=\"" <> p <> "\">") images) <>
        "</body>"
    ["img", x] ->
      respond $ responseFile status200 [(CI.mk "content-type", "image/jpeg")]
      (unpack . fromStrict $ "./img/" <> x) Nothing
    ["style.css"] ->
      respond $ responseFile status200 [(CI.mk "content-type", "text/css")]
      "./style.css" Nothing
    _ ->
      respond $ responseLBS status404 [(CI.mk "content-type", "text/plain")]
      "nyaan"
