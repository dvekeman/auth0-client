{-# LANGUAGE DeriveGeneric #-}
module Auth0.Internal.Types.UserInfo where

import           Data.Aeson
import           Data.Text ( Text )

import           GHC.Generics

data UserInfo = UserInfo
  { email_verified :: Bool
  , email          :: Text
  , updated_at     :: Text
  , name           :: Text
  , picture        :: Text
  , user_id        :: Text
  , nickname       :: Text
  , created_at     :: Text
  , sub            :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON UserInfo
instance ToJSON   UserInfo