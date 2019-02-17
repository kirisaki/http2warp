{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Data.Text.Lazy hiding (map)
import Data.Text.Lazy.Encoding
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = runTLS (tlsSettings "localhost.pem" "localhost-key.pem") defaultSettings app

images :: [LBS.ByteString]
images =  map (\c -> encodeUtf8 $ pack ("img/" <> [c] <> ".jpg")) ['a'..'e']

app :: Application
app req respond = do
  -- let http2 = defaultHTTP2Data
        -- { http2dataPushPromise =
            -- map (\p -> defaultPushPromise { promisedPath }) }
  case pathInfo req of
    [] ->
      respond $ responseLBS status200 [(CI.mk "content-type", "text/html")] $
      "<html><head><title>http2warp</title><style>img{width: 100px}</style></head><body>" <>
      mconcat (map (\p -> "<img src=\"" <> p <> "\">") images) <>
      "</body>"
    ["img", x] ->
      respond $ responseFile status200 [(CI.mk "content-type", "image/jpeg")]
      (unpack . fromStrict $ "./img/" <> x) Nothing
    _ ->
      respond $ responseLBS status404 [(CI.mk "content-type", "text/plain")]
      "nyaan"
