module Auth0.Internal.Types.Common.Token where

import           Data.ByteString ( ByteString )
import           Data.Monoid     ( (<>) )
import           Data.Text       ( Text )
import qualified Data.Text.Encoding as Enc8

type Token = Text 

mkToken :: ByteString -> Token
mkToken token = "Bearer " <> Enc8.decodeUtf8 token

