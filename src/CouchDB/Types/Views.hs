{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module CouchDB.Types.Views where


import Control.Lens (makeLensesWith, camelCaseFields)
import Data.Aeson (FromJSON(parseJSON), (.:), (.:?), Value(Object))
import Data.Int (Int64)
import Data.Text (Text)


data ViewQueryParameters k
  = ViewQueryParameters
  { viewQueryParametersKey :: Maybe k
  , viewQueryParametersLimit :: Maybe Int
  }


defaultViewQueryParameters :: ViewQueryParameters k
defaultViewQueryParameters =
  ViewQueryParameters Nothing Nothing


data ViewResult k v
  = ViewResult
  { viewResultOffset :: Int64
  , viewResultRows :: [ViewRow k v]
  , viewResultTotalRows :: Int64
  , viewResultUpdateSeq :: Maybe Int64
  }


data ViewRow k v
  = ViewRow
  { viewRowId_ :: Text
  , viewRowKey :: k
  , viewRowValue :: v
  }


instance (FromJSON k, FromJSON v) => FromJSON (ViewResult k v) where
  parseJSON (Object o) =
    ViewResult <$> (o .: "offset") <*> (o .: "rows") <*> (o .: "total_rows") <*> (o .:? "update_seq")
  parseJSON _ =
    error "Invalid ViewResult JSON"


instance (FromJSON k, FromJSON v) => FromJSON (ViewRow k v) where
  parseJSON (Object o) =
    ViewRow <$> (o .: "id") <*> (o .: "key") <*> (o .: "value")
  parseJSON _ =
    error "Invalid ViewRow JSON"


makeLensesWith camelCaseFields ''ViewQueryParameters
makeLensesWith camelCaseFields ''ViewResult
makeLensesWith camelCaseFields ''ViewRow
