module CouchDB.Auth where


import CouchDB.Types.Auth (Auth(..))
import Network.HTTP.Simple (Request, setRequestBasicAuth)


setAuth :: Auth -> Request -> Request
setAuth NoAuth req =
  req
setAuth (BasicAuth username password) req =
  setRequestBasicAuth username password req
