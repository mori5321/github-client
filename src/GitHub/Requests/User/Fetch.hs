{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module GitHub.Requests.User.Fetch where

import GHC.Generics
import qualified Data.ByteString.Char8 as S8
import GitHub.Types.User ( User(..) )
import GitHub.Request ( Request(..)
                      , Method(..)
                      , Path
                      , mkHttpRequest
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

