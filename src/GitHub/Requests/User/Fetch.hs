{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module GitHub.Requests.User.Fetch
  ( fetchUser
  , fetchUserHttpRequest
  )
where

import GHC.Generics
import qualified Data.ByteString.Char8 as S8

import GitHub.Types.User ( User(..) )
import GitHub.Request ( Request(..)
                      , Method(..)
                      , Path
                      , mkHttpRequest
                      , getResponseBody
                      , sendRequest
                      , sendRequest'
                      , getResponseStatusCode
                      , withAuth
                      , withBody
                      )
import qualified Network.HTTP.Simple as HTTP
import GitHub.Auth (Auth)
import Data.Aeson (Value, FromJSON, ToJSON) 
import GitHub.Error (Error, parseBodyEither, getResponseBodyEither)


type UserName = S8.ByteString

mkRequest :: UserName -> Request
mkRequest userName = Request ("/users/" <> userName) GET

fetchUserHttpRequest :: UserName -> Auth -> HTTP.Request
fetchUserHttpRequest name auth = withAuth auth
                                 . mkHttpRequest $ req
  where
    req = mkRequest name

fetchUser :: UserName -> Auth -> IO (Either Error User)
fetchUser name auth =
    getResponseBodyEither <$> sendRequest' httpReq
  where
    httpReq = fetchUserHttpRequest name auth
