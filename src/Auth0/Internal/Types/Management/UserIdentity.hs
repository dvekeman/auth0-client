{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.UserIdentity where

import           Data.Aeson (ToJSON, FromJSON)
import           Data.Text (Text)

import           GHC.Generics

data UserIdentity = UserIdentity
  { user_id :: Maybe Text 
  , provider :: Maybe Text
  , connection :: Maybe Text
  , isSocial :: Maybe Bool
  } deriving (Eq, Show, Generic)
instance ToJSON UserIdentity
instance FromJSON UserIdentity

