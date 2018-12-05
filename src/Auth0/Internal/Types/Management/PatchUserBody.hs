{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.PatchUserBody where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import qualified Data.Map.Strict as M
import           Data.Text (Text)

import           GHC.Generics

data PatchUserBody = PatchUserBody
  { blocked :: Maybe Bool 
  , email_verified :: Maybe Bool
  , email :: Maybe Text
  , verify_email :: Maybe Bool
  , phone_number :: Maybe Text
  , phone_verified :: Maybe Bool
  , verify_phone_number :: Maybe Bool
  , password :: Maybe Text
  , user_metadata :: Maybe (M.Map Text Text)
  , app_metadata :: Maybe (M.Map Text Text)
  , username :: Maybe Text
  , client_id :: Maybe Text
  , connection :: Text
  } deriving (Eq, Show, Generic)
instance ToJSON PatchUserBody where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON PatchUserBody where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }

defaultPatchUserBody :: 
  Text -- ^ connection 
  -> PatchUserBody
defaultPatchUserBody connection = PatchUserBody
  { blocked = Nothing 
  , email_verified = Nothing
  , email = Nothing
  , verify_email = Nothing
  , phone_number = Nothing
  , phone_verified = Nothing
  , verify_phone_number = Nothing
  , password = Nothing
  , user_metadata = Just M.empty
  , app_metadata = Just M.empty
  , username = Nothing
  , client_id = Nothing
  , connection = connection
  }
