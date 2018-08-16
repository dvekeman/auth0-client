{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.PatchUserBody where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import           Data.Text (Text)

import qualified Auth0.Internal.Types.Management.UserMetadata as UM
import qualified Auth0.Internal.Types.Management.AppMetadata as AM

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
  , user_metadata :: Maybe UM.UserMetadata
  , app_metadata :: Maybe AM.AppMetadata
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
defaultPatchUserBody = PatchUserBody
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
