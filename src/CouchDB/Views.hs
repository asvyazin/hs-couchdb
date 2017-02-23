{-# LANGUAGE OverloadedStrings #-}
module CouchDB.Views where


import Blaze.ByteString.Builder (toByteString)
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.IO.Class (MonadIO(liftIO))
import CouchDB.Auth (setAuth)
import CouchDB.Types.Auth (Auth)
import CouchDB.Types.Views (ViewQueryParameters, ViewResult, key, limit)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString.Char8 (unpack, pack, empty)
import Data.ByteString.Lazy (toStrict)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit (HttpExceptionContent(StatusCodeException))
import Network.HTTP.Simple (Request, parseRequest, httpJSONEither, getResponseStatus, getResponseBody, HttpException(HttpExceptionRequest))
import Network.HTTP.Types.QueryLike (toQuery)
import Network.HTTP.Types.Status (ok200)
import Network.HTTP.Types.URI (encodePath)


getView :: (MonadThrow m, MonadIO m, FromJSON k, ToJSON k, FromJSON v) => Text -> Text -> Auth -> Text -> Text -> ViewQueryParameters k -> m (ViewResult k v)
getView couchdbServer databaseName auth designDoc view queryParameters = do
  req <- initRequest couchdbServer databaseName auth designDoc view queryParameters
  resp <- httpJSONEither req
  let responseStatus = getResponseStatus resp
  if responseStatus == ok200
    then
    case getResponseBody resp of
      Left e -> throwM e
      Right res -> return res
    else throwM $ HttpExceptionRequest req $ StatusCodeException (void resp) empty


initRequest :: (MonadThrow m, MonadIO m, ToJSON k) => Text -> Text -> Auth -> Text -> Text -> ViewQueryParameters k -> m Request
initRequest couchdbServer databaseName auth designDoc view parameters = do
  let
    viewUrl = getViewUrl couchdbServer databaseName designDoc view parameters
  liftIO $ putStrLn viewUrl
  req <- parseRequest viewUrl
  return $ setAuth auth req


getViewUrl :: (ToJSON k) => Text -> Text -> Text -> Text -> ViewQueryParameters k -> String
getViewUrl couchdbServer databaseId designDoc view parameters =
  unpack $ encodeUtf8 couchdbServer <> toByteString (encodePath [databaseId, "_design", designDoc, "_view", view] query)
  where
    query =
      toQuery [ keyParam <$> parameters ^. key
              , limitParam <$> parameters ^. limit
              ]
    keyParam k = (keyParamName, toStrict (encode k))
    limitParam l = (limitParamName, pack (show l))


keyParamName :: Text
keyParamName = "key"


limitParamName :: Text
limitParamName = "limit"
