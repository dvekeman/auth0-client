{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth0.Internal.ManagementApi where

import           Data.Text (Text)
import           Servant.API

import           Auth0.Internal.Types

type ManagementApi = 
  ConnectionApi 
  :<|> UsersApi

type ConnectionApi = 
  -- GET /api/v2/connections
  Header "Authorization" Token :> "api" :> "v2" :> "connections" :> Get '[JSON] [Connection]

type UsersApi = 
  -- GET /api/v2/users List or search users
  Header "Authorization" Token :> "api" :> "v2" :> "users"
    :> QueryParam "per_page" Int
    :> QueryParam "page" Int
    :> QueryParam "include_totals" Bool
    :> QueryParam "sort" Text
    :> QueryParam "connection" Text
    :> QueryParam "fields" Text
    :> QueryParam "include_fields" Bool
    :> QueryParam "q" Text
    :> QueryParam "search_engine" Text
    :> Get '[JSON] GetUsersResponse
  -- GET /api/v2/user/{id} Get a user
  :<|> Header "Authorization" Token :> "api" :> "v2" :> "users" :> Capture "id" Text :> Get '[JSON] User 
  -- POST /api/v2/users Create a user
  :<|> Header "Authorization" Token :> "api" :> "v2" :> "users" :> ReqBody '[JSON] PostUserBody :> Post '[JSON] User   
  -- PATCH /api/v2/users/{id} Update a user
  :<|> Header "Authorization" Token :> "api" :> "v2" :> "users" :> Capture "id" Text :> ReqBody '[JSON] PatchUserBody :> Patch '[JSON] User
  -- DELETE /api/v2/users/{id} Delete a user
  :<|> Header "Authorization" Token :> "api" :> "v2" :> "users" :> Capture "id" Text :> Delete '[JSON] ()
  -- GET /api/v2/users-by-email Search a user by email 
  :<|> Header "Authorization" Token :> "api" :> "v2" :> "users-by-email" :> QueryParam "email" Text :> Get '[JSON] [User]