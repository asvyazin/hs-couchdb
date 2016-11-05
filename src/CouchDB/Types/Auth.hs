module CouchDB.Types.Auth where


import Data.ByteString (ByteString)


data Auth
  = BasicAuth ByteString ByteString
  | NoAuth deriving (Show)
