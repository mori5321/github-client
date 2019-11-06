{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GitHub.Requests.Issues.Create where

import GHC.Generics
import qualified Data.Text as T
import qualified Network.HTTP.Simple as HTTP
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Text as T

import GitHub.Request ( Request(..)
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
import GitHub.Auth ( Auth )
import GitHub.Error (Error, parseBodyEither, getResponseBodyEither)
import GitHub.Query ( toHTTPQueryItem, QueryItem )
import GitHub.Types.Issue (Issue)



data ReqBody = ReqBody { title :: T.Text
                       , body :: T.Text
                       } deriving (Show, Generic, ToJSON, FromJSON)

type OwnerName = T.Text
type RepoName = T.Text

createIssueHttpRequest :: OwnerName -> RepoName -> ReqBody -> Auth -> HTTP.Request
createIssueHttpRequest ownerName repoName body auth =
    withAuth auth . withBody body . mkHttpRequest $ req
  where
    req = Request { reqPath = T.intercalate "/" ["repos", ownerName, repoName, "issues"]
                  , reqMethod = POST
                  }

createIssue :: OwnerName -> RepoName -> ReqBody -> Auth -> IO (Either Error Issue)
createIssue ownerName repoName body auth =
    getResponseBodyEither <$> sendRequest' httpReq
  where
    httpReq = createIssueHttpRequest ownerName repoName body auth