{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
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
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml content
  where
    content = do
      H.docType
      H.html $ do
        H.head $ H.title "Internal Server Error"
        H.body $ do
          H.h1 "Thingy"
          H.form ! A.formaction action $ do
            H.label ! A.for "uri" $ "URI:"
            H.input ! A.name "uri" ! A.type_ "text"
            H.button ! A.type_ "submit" $ "Go!"
    action = "/some/thing"


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
