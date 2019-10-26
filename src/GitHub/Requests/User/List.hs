{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module GitHub.Requests.User.List where

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
                      , mkHttpRequest
                      , withAuth
                      , withBody
                      )
import qualified Network.HTTP.Simple as HTTP
import GitHub.Auth ( Auth )

-- data ReqBody = ReqBody deriving (Show, Generic, ToJSON, FromJSON)
-- listUsersHttpRequest' :: Since -> Auth -> HTTP.Request
-- listUsersHttpRequest' since auth　= withAuth auth . withBody body . mkHttpRequest $ req
--   where
--     req = mkRequest since
--     body = ReqBody


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
    