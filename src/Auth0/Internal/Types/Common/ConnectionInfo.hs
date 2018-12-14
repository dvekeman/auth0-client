module Auth0.Internal.Types.Common.ConnectionInfo where

import           Data.ByteString (ByteString)
import           Data.Monoid ( (<>) )
import           Data.Text (Text)
import           Servant.Client (Scheme(..))

data ConnectionInfo = ConnectionInfo
  { cAuthDomain :: Text
  , cReqDomain :: Text
  , cAudience :: Text
  , cScheme :: Scheme
  , cPort :: Int
  , cPath :: Text
  , cClientId :: ByteString
  , cClientSecret :: ByteString
  } deriving Show

defaultConnectionInfo :: Text -> ConnectionInfo
defaultConnectionInfo cDomain = 
  ConnectionInfo
    cDomain
    cDomain
    ("https://" <> cDomain <> "/api/v2/")
    Https
    443
    ""
    ""
    ""

defaultAuthorizationExtensionConnectionInfo :: 
  Text -- ^ Domain used to acquire an oauth token *.auth.com 
  -> Text -- ^ Domain used to execute the requests *.webtask.io
  -> ConnectionInfo 
defaultAuthorizationExtensionConnectionInfo authDomain reqDomain = 
  (defaultConnectionInfo authDomain)  
    { cReqDomain = reqDomain
    , cAudience = "urn:auth0-authz-api" 
    }