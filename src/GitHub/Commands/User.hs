{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GitHub.Commands.User where

import Data.Text as T
import qualified Data.ByteString.Char8 as S8
import Data.Aeson (FromJSON, ToJSON, decode, Value)
import GHC.Generics

import GitHub.Request ( Request(..)
                      , execRequest
                      , Method(..)
                      , Path
                      , getResponseBody
                      )
import GitHub.Auth (authorize, Auth)

type UserName = S8.ByteString -- Text周り知りたい


-- data AuthorizedUser = AuthorizedUser { id :: Integer
--                                      , login :: T.Text
--                                      , avatar_url :: T.Text
--                                      } deriving (Show, Generic)
-- instance FromJSON AuthorizedUser
-- instance ToJSON AuthorizedUser

type Since = Int
data User = User { id :: Integer
                 , login :: T.Text
                 } deriving (Show, Generic)
instance FromJSON User
instance ToJSON User

fetchUserRequest :: UserName -> Request
fetchUserRequest userName = Request ("/users/" <> userName) GET


fetchUser ::  Auth -> UserName -> IO User
fetchUser auth userName = do
    let req = fetchUserRequest userName
    response <- execRequest req auth
    let user = getResponseBody response :: User
    return user


fetchUsersRequest :: Since -> Request
fetchUsersRequest since = Request ("/users" <> "?since=" <> sinceString) GET
    where
        sinceString = S8.pack $ show since
    
fetchUsers :: Auth -> Since -> IO [User]
fetchUsers auth since = do
    let req = fetchUsersRequest since
    response <- execRequest req auth
    let users = getResponseBody response :: [User]
    return users
