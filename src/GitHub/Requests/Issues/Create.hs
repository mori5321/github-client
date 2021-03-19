{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GitHub.Requests.Issues.Create where

import           GHC.Generics
import qualified Network.HTTP.Simple           as HTTP
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import qualified Data.Text                     as T

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
                                                , toPath
                                                )
import           GitHub.Auth                    ( Auth )
import           GitHub.Error                   ( Error
                                                , parseBodyEither
                                                , getResponseBodyEither
                                                )
import           GitHub.Query                   ( toHTTPQueryItem
                                                , QueryItem
                                                )
import           GitHub.Types.Issue             ( Issue )
import           GitHub.Types.Owner             ( Owner )
import           GitHub.Types.Name              ( Name )
import           GitHub.Types.Repo              ( Repo )

data ReqBody = ReqBody { title :: T.Text
                       , body :: T.Text
                       } deriving (Show, Generic, ToJSON, FromJSON)

type OwnerName = T.Text
type RepoName = T.Text

createIssueHttpRequest
    :: Name Owner -> Name Repo -> ReqBody -> Auth -> HTTP.Request
createIssueHttpRequest ownerName repoName body auth =
    withAuth auth . withBody body . mkHttpRequest $ req
  where
    req = Request
        { reqPath   = T.intercalate
                          "/"
                          ["repos", toPath ownerName, toPath repoName, "issues"]
        , reqMethod = POST
        }

createIssue
    :: Name Owner -> Name Repo -> ReqBody -> Auth -> IO (Either Error Issue)
createIssue ownerName repoName body auth = getResponseBodyEither
    <$> sendRequest' httpReq
    where httpReq = createIssueHttpRequest ownerName repoName body auth
