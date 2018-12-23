module Auth0Client
  ( module Auth0.Internal.Api
  , module Auth0.Internal.ApiClient   
  , module Auth0.Internal.Types
  ) where

import Auth0.Internal.Api
import Auth0.Internal.ApiClient 
  ( Auth0ApiResponse
  -- * Management API
  -- -- * Connections
  , getConnections
  -- -- * Users
  , getUsers
  , getUser
  , createUser
  , updateUser
  , deleteUser
  , getUsersByEmail
  -- * Authentication API
  -- -- * Authorization
  , requestClientCredentialsToken
  , requestAuthorizationToken
  -- -- * Authorization Extension
  , getAllGroups
  , getAllGroupMembers
  , getAllRoles
  , addRoles
  -- -- * Profile
  , getUserInfo
  )
import Auth0.Internal.Types
