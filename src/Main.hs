{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Data.ByteString.Lazy as L
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)


main :: IO ()
main = quickHttpServe site

{-|

<form method="GET" action="{{ encode_rss_action }}">
<label>URI: <input type="text" name="uri"/></label>
<button type='submit'>Go!</button>
</form>
{% if encoded %}
<p>Encoded feed link is: <a href="{{encoded}}">{{encoded}}</a>
{% endif %}

|-}

indexPage :: Snap()
indexPage = do
    uri <-  getParam "uri"
    let uri2 = fmap decodeUtf8 uri

    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ response uri2
  where
    content uri = do
      H.docType
      H.html $ do
        H.head $ H.title "Internal Server Error"
        H.body $ do
          H.h1 "Thingy"
          H.form $ do
            H.label ! A.for "uri" $ "URI:"
            H.input ! A.name "uri" ! A.type_ "text"
            H.button ! A.type_ "submit" $ "Go!"
          H.p $ maybe noUri hasUri uri

    noUri = H.span "No url"
    hasUri uri = H.p $ do
        H.span "URI: "
        H.toHtml uri
    response :: Maybe Text -> L.ByteString
    response uri = renderHtml $ content uri


site :: Snap ()
site =
    ifTop indexPage <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
