{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.ClientToken where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text.Encoding as Enc8

import           GHC.Generics

data ClientToken = ClientToken
  { access_token :: Text -- Ideally ByteString but there is no Aeson instance for it..
  , scope        :: Text
  , expires_in   :: Int
  , token_type   :: Text
  , id_token     :: Maybe Text
  } deriving (Eq, Show, Generic)
instance ToJSON ClientToken where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON ClientToken where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }

mkClientToken :: Text -> Text -> Int -> Text -> Maybe Text -> ClientToken
mkClientToken = ClientToken

accessToken :: ClientToken -> BS.ByteString
accessToken = Enc8.encodeUtf8 . access_token