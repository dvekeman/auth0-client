{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.User where

import           Data.Aeson (ToJSON, FromJSON)
import           Data.Text (Text)

import qualified Auth0.Internal.Types.Management.UserIdentity as I
import qualified Auth0.Internal.Types.Management.UserMetadata as UM
import qualified Auth0.Internal.Types.Management.AppMetadata as AM

import           GHC.Generics

data User = User 
  { email :: Maybe Text
  , email_verified :: Maybe Bool
  , username :: Maybe Text
  , phone_number :: Maybe Text
  , phone_verified :: Maybe Bool
  , user_id :: Maybe Text
  , created_at :: Maybe Text
  , updated_at :: Maybe Text
  , identities :: [ I.UserIdentity ]
  , app_metadata :: Maybe AM.AppMetadata
  , user_metadata :: Maybe UM.UserMetadata
  , picture :: Maybe Text
  , name :: Maybe Text
  , nickname :: Maybe Text
  , multifactor :: Maybe [ Text ]
  , last_ip :: Maybe Text
  , last_login :: Maybe Text
  , logins_count :: Maybe Int
  , blocked :: Maybe Bool
  , given_name :: Maybe String
  , family_name :: Maybe String
  } deriving (Eq, Show, Generic)
instance ToJSON User
instance FromJSON User
  
data Identity = Identity 
  { 
  } deriving (Eq, Show, Generic)
instance ToJSON Identity
instance FromJSON Identity

