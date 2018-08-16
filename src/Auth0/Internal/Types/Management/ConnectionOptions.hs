{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.ConnectionOptions where

import           Data.Aeson (ToJSON, FromJSON)
import           Data.Text (Text)

import           GHC.Generics

data ConnectionOptions = ConnectionOptions
  { mfa :: Maybe MultiFactorAuthentication
  , passwordPolicy :: Maybe Text
  , brute_force_protection :: Maybe Bool
  , strategy_version :: Maybe Int
  , requires_username :: Maybe Bool
  , validation :: Maybe Validation
  , email :: Maybe Bool
  , profile :: Maybe Bool
  , scope :: Maybe [Text]
  } deriving (Eq, Show, Generic)
instance ToJSON ConnectionOptions
instance FromJSON ConnectionOptions

data MultiFactorAuthentication = MultiFactorAuthentication
  { active :: Bool
  , return_enroll_settings :: Bool
  } deriving (Eq, Show, Generic)
instance ToJSON MultiFactorAuthentication
instance FromJSON MultiFactorAuthentication

data Validation = Validation 
  { username :: Maybe UsernameValidation
  } deriving (Eq, Show, Generic)
instance ToJSON Validation
instance FromJSON Validation

data UsernameValidation = UsernameValidation
  { min :: Int
  , max :: Int
  } deriving (Eq, Show, Generic)
instance ToJSON UsernameValidation
instance FromJSON UsernameValidation