{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module CouchDB.Changes.Watcher where


import CouchDB.Types.Seq (Seq(IntSeq, TextSeq, EmptySeq))
import Control.Applicative ((<|>))
import Control.Lens (makeLenses)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Blaze.ByteString.Builder (toByteString)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?), json')
import Data.Aeson.Types (parseEither)
import Data.ByteString.Char8 (pack, unpack)
import Data.Conduit ((=$=), Sink, ConduitM, fuseBoth, await, yield)
import Data.Conduit.Attoparsec (conduitParser)
import qualified Data.Conduit.Combinators as DC (map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.URI (renderQuery, encodePathSegments)
import Network.HTTP.Simple (parseRequest, httpSink)


watchChanges :: (MonadIO m, MonadMask m) => WatchParams -> Sink DocumentChange m a -> m a
watchChanges params consumer = do
  (maybeLs, result) <- watchChanges' params consumer
  case maybeLs of
    Nothing ->
      return result
    Just ls ->
      watchChanges params { _since = Just ls } consumer


watchChanges' :: (MonadIO m, MonadMask m) => WatchParams -> Sink DocumentChange m a -> m (Maybe Seq, a)
watchChanges' params consumer = do
  let
    sinceParam s =
      ("since", Just $ seqToString s)

    seqToString EmptySeq = ""
    seqToString (IntSeq i64) =
      pack $ show i64
    seqToString (TextSeq str) = encodeUtf8 str

    filterParam flt =
      ("filter", Just $ encodeUtf8 flt)

    qsParams =
      [ Just ("feed", Just "continuous")
      , sinceParam <$> _since params
      , filterParam <$> _filter params
      ]

    qs =
      renderQuery True $ catMaybes qsParams

    url =
      encodeUtf8 (_baseUrl params) <> toByteString (encodePathSegments [_database params, "_changes"]) <> qs

    findLastSeq :: Monad m => ConduitM (Either String WatchItem) DocumentChange m (Maybe Seq)
    findLastSeq = do
      maybeRes <- await
      case maybeRes of
        Nothing ->
          return Nothing
        Just res ->
          case res of
            Left e ->
              fail e
            Right (LastSeqItem (LastSeq ls)) ->
              return $ Just ls
            Right (DocumentChangeItem i) -> do
              yield i
              findLastSeq

  req <- parseRequest $ unpack url
  httpSink req $ const $ conduitParser json' =$= DC.map (parseEither parseJSON . snd) =$= findLastSeq `fuseBoth` consumer


data WatchParams =
  WatchParams
  { _baseUrl :: Text
  , _database :: Text
  , _since :: Maybe Seq
  , _filter :: Maybe Text
  }


data WatchItem
  = DocumentChangeItem DocumentChange
  | LastSeqItem LastSeq
  deriving (Show)


data DocumentChange =
  DocumentChange
  { __seq :: Seq
  , __id :: Text
  , _changes :: [Change]
  , _deleted :: Bool
  } deriving (Show)


data Change =
  Change
  { _rev :: Text
  } deriving (Show)


instance FromJSON DocumentChange where
  parseJSON (Object v) =
    DocumentChange <$> v .: "seq" <*> v .: "id" <*> v .: "changes" <*> (fromMaybe False <$> v .:? "deleted")
  parseJSON _ =
    error "Invalid DocumentChange JSON"


instance FromJSON Change where
  parseJSON (Object v) =
    Change <$> v .: "rev"
  parseJSON _ =
    error "Invalid Change JSON"


instance FromJSON WatchItem where
  parseJSON x =
    (DocumentChangeItem <$> parseJSON x) <|> (LastSeqItem <$> parseJSON x)


data LastSeq =
  LastSeq
  { _lastSeq :: Seq
  } deriving (Show)


instance FromJSON LastSeq where
  parseJSON (Object o) =
    LastSeq <$> o .: "last_seq"
  parseJSON _ =
    error "Invalid LastSeq JSON"


makeLenses ''DocumentChange
makeLenses ''Change
makeLenses ''LastSeq
