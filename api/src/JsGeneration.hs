{-# LANGUAGE OverloadedStrings #-}

-- Custom JS client generation
module JsGeneration where

import           Control.Lens
import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import           Data.Text.IO        as T (writeFile)
import           Servant.Foreign
import           Servant.JS
import           Servant.JS.Internal

import Api

writeJsClient :: T.Text -> FilePath -> IO ()
writeJsClient host path = do
  let jsApi = jsForAPI myApiProxy .  reactWith $
                defCommonGeneratorOptions { urlPrefix = host }

  T.writeFile path jsApi

-- An example of how to customize JS client API generation. It uses the
-- (relatively) new `fetch` browser API, and exposes the API as a class so that
-- authorization data can be supplied once in the constructor rather than
-- passed to every function.
--
-- It's a little bit janky. With some thought, the headers/value that are
-- provided in the constructor could be made more generic.
--
-- To modify further, see existing implementations at:
-- https://github.com/haskell-servant/servant-js/tree/master/src/Servant/JS
--
-- WARNING: This is a prototype and not complete! In particular it doesn't
-- handle request bodies, and likely other common needs.
react :: JavaScriptGenerator
react = reactWith defCommonGeneratorOptions

reactWith :: CommonGeneratorOptions -> JavaScriptGenerator
reactWith opts = \reqs ->
     "class Api {\n"
  <> "  constructor(jwt) { this.jwt = jwt }\n\n"
  <> (mconcat . map (generateReactJSWith opts) $ reqs)
  <> "}\n"
  <> "export { Api as default }"

generateReactJSWith :: CommonGeneratorOptions -> AjaxReq -> T.Text
generateReactJSWith opts req =
       fname <> "(" <> argsStr <> ") {\n"
    <> "  return fetch(" <> url <> ", { bogus: true\n"
    <> reqheaders
    <> "  });\n"
    <> "}\n\n"
  where
    argsStr = T.intercalate ", " args
    args = captures
        ++ map (view $ queryArgName . argPath) queryparams
    captures = map (view argPath . captureArg)
                 . filter isCapture
                 $ req ^. reqUrl.path

    hs = req ^. reqHeaders

    reqheaders =
      if null hs
        then ""
        else "    , headers: { " <> headersStr <> " }\n"

      where
        headersStr = T.intercalate ", " $ map headerStr hs
        headerStr header =
          let headerName = header ^. headerArg . argPath in
          let headerValue = case headerName of
                              "Authorization" -> "\"Bearer \" + this.jwt"
                              _               -> toJSHeader header
                            in

          "\"" <> headerName <> "\": " <> headerValue

    fname = (functionNameBuilder opts $ req ^. reqFuncName)
    queryparams = req ^.. reqUrl.queryStr.traverse
    url = if url' == "'" then "'/'" else url'
    url' = "'"
       <> urlPrefix opts
       <> urlArgs
       <> queryArgs

    urlArgs = jsSegments
            $ req ^.. reqUrl.path.traverse

    queryArgs = if null queryparams
                  then ""
                  else " + '?" <> jsParams queryparams
