{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.PostUserBody where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import qualified Data.Map.Strict as M
import           Data.Text (Text)

import           GHC.Generics

data PostUserBody = PostUserBody
  { user_id :: Maybe Text
  , email :: Maybe Text
  , email_verified :: Maybe Bool
  , verify_email :: Maybe Bool
  , username :: Maybe Text
  , password :: Maybe Text
  , user_metadata :: Maybe (M.Map Text Text)
  , app_metadata :: Maybe (M.Map Text Text)
  , connection :: Text
  } deriving (Eq, Show, Generic)
instance ToJSON PostUserBody where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON PostUserBody where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }

defaultPostUserBody :: 
  Text -- ^ Connection 
  -> PostUserBody
defaultPostUserBody connection = PostUserBody
  { user_id = Nothing
  , email = Nothing
  , email_verified = Nothing
  , verify_email = Nothing
  , username = Nothing
  , password = Nothing
  , user_metadata = Just M.empty
  , app_metadata = Just M.empty
  , connection = connection
  }
 