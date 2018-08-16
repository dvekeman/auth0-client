module Auth0.Internal.Types.Common.ConnectionInfo where

import           Data.Aeson (ToJSON, FromJSON)
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Servant.Client (Scheme(..))

import           GHC.Generics

data ConnectionInfo = ConnectionInfo
  { cDomain :: Text
  , cScheme :: Scheme
  , cPort :: Int
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