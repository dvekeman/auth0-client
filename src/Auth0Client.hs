module Auth0Client
  ( Auth0Api
  , Token
  , mkToken
  , api
  
  -- * /userinfo
  , UserInfo
  , userinfo
   
  -- * Servant stuff
  , ServantError
  ) where

import Auth0.Internal.Api ( Auth0Api, Token, mkToken, api, userinfo )
import Auth0.Internal.Types.UserInfo
import Servant.Client     ( ServantError )