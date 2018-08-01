{-# LANGUAGE DeriveGeneric #-}
module Auth0.Internal.Types.UserInfo where

import           Data.Aeson
import           Data.Text ( Text )

import           GHC.Generics

data UserInfo = UserInfo
  { email_verified :: Maybe Bool
  , email          :: Maybe Text
  , updated_at     :: Text
  , name           :: Text
  , picture        :: Text
  , user_id        :: Maybe Text
  , nickname       :: Text
  , created_at     :: Maybe Text
  , sub            :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON UserInfo
instance ToJSON   UserInfo