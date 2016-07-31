{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module CouchDB.Requests where


import Blaze.ByteString.Builder (toByteString)
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (unpack)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (responseCookieJar)
import Network.HTTP.Simple (parseRequest
                           , httpJSONEither
                           , getResponseStatus
                           , getResponseBody
                           , getResponseHeaders
                           , HttpException(StatusCodeException)
                           , setRequestMethod
                           , setRequestBodyJSON
                           , httpLBS)
import Network.HTTP.Types.Status (ok200, notFound404)
import Network.HTTP.Types.URI (encodePathSegments)


getObjectUrl :: Text -> Text -> Text -> String
getObjectUrl couchdbServer databaseId objectId =
  unpack $ encodeUtf8 couchdbServer <> toByteString (encodePathSegments [databaseId, objectId])


getObject :: (MonadThrow m, MonadIO m, FromJSON a) => Text -> Text -> Text -> m (Maybe a)
getObject couchdbServer databaseName objectId = do
  req <- parseRequest $ getObjectUrl couchdbServer databaseName objectId
  resp <- httpJSONEither req
  let responseStatus = getResponseStatus resp
  if responseStatus == ok200
    then
    case getResponseBody resp of
      Left e -> throwM e
      Right res -> return $ Just res
    else
    if responseStatus == notFound404
    then return Nothing
    else throwM $ StatusCodeException responseStatus (getResponseHeaders resp) (responseCookieJar resp)


putObject :: (MonadThrow m, MonadIO m, ToJSON a) => Text -> Text -> Text -> a -> m ()
putObject couchdbServer databaseName objectId obj = do
  initReq <- parseRequest $ getObjectUrl couchdbServer databaseName objectId
  let
    req =
      setRequestMethod "PUT" $ setRequestBodyJSON obj initReq
  void $ httpLBS req
