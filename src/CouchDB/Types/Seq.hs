module CouchDB.Types.Seq where


import Data.Aeson (Value(String, Number, Null), FromJSON(parseJSON), ToJSON(toJSON))
import Data.Int (Int64)
import Data.Text (Text)


data Seq
  = EmptySeq
  | IntSeq Int64
  | TextSeq Text
  deriving (Show)


instance FromJSON Seq where
  parseJSON (String str) =
    return $ TextSeq str
  parseJSON v@(Number _) =
    IntSeq <$> parseJSON v
  parseJSON _ =
    error "Invalid seq value"


instance ToJSON Seq where
  toJSON EmptySeq = Null
  toJSON (IntSeq i64) = Number $ fromIntegral i64
  toJSON (TextSeq str) = String str
