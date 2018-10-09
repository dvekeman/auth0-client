module Auth0.Internal.Types.Common.ConnectionInfo where

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Servant.Client (Scheme(..))

data ConnectionInfo = ConnectionInfo
  { cDomain :: Text
  , cScheme :: Scheme
  , cPort :: Int
  , cPath :: Text
  , cClientId :: ByteString
  , cClientSecret :: ByteString
  }

defaultConnectionInfo :: Text -> ConnectionInfo
defaultConnectionInfo cDomain = 
  ConnectionInfo
    cDomain
    Https
    443
    ""
    ""
    ""
