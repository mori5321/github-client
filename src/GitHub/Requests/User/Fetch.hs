{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module GitHub.Requests.User.Fetch where

import Data.Text as T
import qualified Data.ByteString.Char8 as S8
import Data.Aeson ( Value, FromJSON, ToJSON )
import GHC.Generics
import GitHub.Types.User ( User(..) )
import GitHub.Request ( IsRequest(..)
                      , Request(..)
                      , Method(..)
                      , Path
                      , getResponseBody
                      , sendRequest
                      , getResponseStatusCode
                      , withAuth
                      , withBody
                      )
import qualified Network.HTTP.Simple as HTTP
import GitHub.Auth ( Auth )

type UserName = S8.ByteString

mkRequest :: UserName -> Request
mkRequest userName = Request ("/users/" <> userName) GET


fetchUser :: UserName -> Auth -> IO User
fetchUser name auth =
    getResponseBody <$> sendRequest (withAuth auth . mkHttpRequest $ req)
  where
    req = mkRequest name

