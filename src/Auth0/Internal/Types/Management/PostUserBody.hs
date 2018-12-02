{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.PostUserBody where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import qualified Data.Map.Strict as M
import           Data.Text (Text)

import qualified Auth0.Internal.Types.Management.UserMetadata as UM
import qualified Auth0.Internal.Types.Management.AppMetadata as AM

import           GHC.Generics

-- TODO: phone_number should be present (but only for SMS connections...)

data PostUserBody = PostUserBody
  { user_id :: Maybe Text
  , email :: Maybe Text
  , email_verified :: Maybe Bool
  , verify_email :: Maybe Bool
  , username :: Maybe Text
  , password :: Maybe Text
  , user_metadata :: M.Map String String
  , app_metadata :: M.Map String String
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
defaultPostUserBody = PostUserBody
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  M.empty
  M.empty
 