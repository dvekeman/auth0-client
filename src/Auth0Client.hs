module Auth0Client
  ( Auth0Api
  , Token
  , mkToken
  , api
  
  -- * 
  , getUserInfo
  -- * /userinfo
  , UserInfo(..)
  , userinfo
   
  -- * Servant stuff
  , ServantError
  ) where

import Auth0.Internal.Api ( Auth0Api, Token, mkToken, api, userinfo )
import Auth0.Internal.ApiClient (getUserInfo)
import Auth0.Internal.Types.UserInfo
import Servant.Client     ( ServantError )