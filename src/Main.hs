{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Data.ByteString.Lazy as L
import           Data.ByteString as B
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import           Crypto.Hash.Algorithms (Blake2s_256)
import           Crypto.Hash (Digest)
import           Data.ByteArray.Encoding (convertToBase, Base(Base64URLUnpadded))


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
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ response uri
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
    hasUri uri = do
      H.p $ do
        H.span "URI: "
        H.toHtml $ decodeUtf8 uri
      H.p $ do
        H.span "Mac: "
        H.toHtml $ signUri uri
    response uri = renderHtml $ content uri


signUri :: B.ByteString -> Text

signUri uri = undigest
  where
    key = ("FIXME" :: B.ByteString)
    mac :: Digest Blake2s_256
    mac = hmacGetDigest $ hmac key uri

    undigest = decodeUtf8 $ convertToBase Base64URLUnpadded mac

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
