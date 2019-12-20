{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module GitHub.Requests.User.List
        ( listUsers
        )
where

import           GHC.Generics
import qualified Data.Text                     as T
import qualified Network.HTTP.Simple           as HTTP

import           GitHub.Types.User              ( User(..) )
import           GitHub.Request                 ( Request(..)
                                                , Method(..)
                                                , getResponseBody
                                                , sendRequest
                                                , sendRequest'
                                                , getResponseStatusCode
                                                , mkHttpRequest
                                                , withAuth
                                                , withBody
                                                , withQuery
                                                )
import           GitHub.Auth                    ( Auth )
import           GitHub.Error                   ( Error
                                                , parseBodyEither
                                                , getResponseBodyEither
                                                )
import           GitHub.Query                   ( toHTTPQueryItem
                                                , QueryItem
                                                )


type Since = Integer
mkRequest :: Request
mkRequest = Request path GET where path = mconcat ["users"]

listUsersHttpRequest :: [QueryItem] -> HTTP.Request
listUsersHttpRequest queryItems = withQuery query . mkHttpRequest $ req
    where
        req   = mkRequest
        query = map toHTTPQueryItem queryItems


-- listUsers :: Since -> IO [User]
-- listUsers since =
--     getResponseBody <$> sendRequest httpReq
--   where
--     httpReq = listUsersHttpRequest since

listUsers :: [QueryItem] -> IO (Either Error [User])
listUsers since = getResponseBodyEither <$> sendRequest' httpReq
        where httpReq = listUsersHttpRequest since
