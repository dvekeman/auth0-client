module Auth0.Internal.Types.Management.GetUsersParams where

import Data.Text (Text)

data GetUsersParams =
  GetUsersParams
    { perPage :: Maybe Int
    , page :: Maybe Int
    , includeTotals :: Maybe Bool
    , sort :: Maybe Text
    , connection :: Maybe Text
    , fields :: Maybe Text
    , includeFields :: Maybe Bool
    , q :: Maybe Text
    , searchEngine :: Maybe Text
    } deriving (Eq, Show)

defaultGetUsersParams :: GetUsersParams
defaultGetUsersParams =
  GetUsersParams
    { perPage = Nothing
    , page = Nothing
    , includeTotals = Just True
    , sort = Nothing
    , connection = Nothing
    , fields = Nothing
    , includeFields = Nothing
    , q = Nothing
    , searchEngine = Nothing
    }

