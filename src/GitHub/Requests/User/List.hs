{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module GitHub.Requests.User.List where

import GHC.Generics
import qualified Data.ByteString.Char8 as S8
import qualified Network.HTTP.Simple as HTTP


import GitHub.Types.User ( User(..) )
import GitHub.Request ( Request(..)
                      , Method(..)
                      , getResponseBody
                      , sendRequest
                      , getResponseStatusCode
                      , mkHttpRequest
                      , withAuth
                      , withBody
                      )
import GitHub.Auth ( Auth )


type Since = Integer
mkRequest :: Since -> Request
mkRequest since = Request path GET
  where
    path = mconcat ["users", "?since=", sinceString]
    sinceString = S8.pack $ show since

listUsersHttpRequest :: Since -> HTTP.Request
listUsersHttpRequest since = mkHttpRequest req
  where
    req = mkRequest since

listUsers :: Since -> IO [User]
listUsers since =
    getResponseBody <$> sendRequest httpReq
  where
    httpReq = listUsersHttpRequest since
    