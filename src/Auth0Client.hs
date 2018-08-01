module Auth0Client
  ( Auth0Api
  , Token
  , mkToken
  , api
  
  -- * /userinfo
  , UserInfo
  , userinfo 
  ) where

import Auth0.Internal.Api ( Auth0Api, Token, mkToken, api, userinfo )
import Auth0.Internal.Types.UserInfo