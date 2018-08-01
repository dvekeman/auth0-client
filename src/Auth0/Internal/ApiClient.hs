module Auth0.Internal.ApiClient where

import qualified Data.ByteString as BS
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client (runClientM, ClientEnv(..), ServantError, Scheme(..), BaseUrl(..))

import Auth0.Internal.Api
import Auth0.Internal.Types.UserInfo

getUserInfo :: BS.ByteString -> IO (Either ServantError UserInfo)
getUserInfo token = do
  manager' <- newManager tlsManagerSettings
  runClientM (userinfo . Just $ mkToken token) (ClientEnv manager' (BaseUrl Https "hadruki.eu.auth0.com" 443 ""))
