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
                      , sendRequest'
                      , getResponseStatusCode
                      , withAuth
                      , withBody
                      )
import qualified Network.HTTP.Simple as HTTP
import GitHub.Auth (Auth)
import Data.Aeson (Value) 
import GitHub.Error (Error, parseError)

type UserName = S8.ByteString

mkRequest :: UserName -> Request
mkRequest userName = Request ("/users/" <> userName) GET



fetchUser :: UserName -> Auth -> IO (Either Error User)
fetchUser name auth = do
    body <- getResponseBody
            <$> sendRequest' (withAuth auth . mkHttpRequest $ req)

    case body of
      Left error -> return $ Left $ parseError error
      Right user -> return $ Right user
  where
    req = mkRequest name

