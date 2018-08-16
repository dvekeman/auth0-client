{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.PostUserBody where

import           Data.Aeson (ToJSON, FromJSON)
import           Data.ByteString (ByteString)
import           Data.Text (Text)

import qualified Auth0.Internal.Types.Management.UserMetadata as UM
import qualified Auth0.Internal.Types.Management.AppMetadata as AM

import           GHC.Generics

data PostUserBody = PostUserBody
  { user_id :: Maybe Text
  , connection :: Text
  , email :: Maybe Text
  , username :: Maybe Text
  , password :: Maybe Text
  , user_metadata :: Maybe UM.UserMetadata
  , email_verified :: Maybe Bool
  , verify_email :: Maybe Bool
  , app_metadata :: Maybe AM.AppMetadata
  } deriving (Eq, Show, Generic)
instance ToJSON PostUserBody
instance FromJSON PostUserBody

defaultPostUserBody :: Text -> PostUserBody
defaultPostUserBody connection = PostUserBody
  Nothing
  connection
  Nothing
  Nothing
  Nothing
--   Nothing
--   Nothing
  Nothing
  Nothing
  Nothing
  Nothing
 
