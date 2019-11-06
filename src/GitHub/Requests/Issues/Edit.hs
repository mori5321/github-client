{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GitHub.Requests.Issues.Edit where

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
                       , state :: T.Text -- この辺もうちょっと型で強くしたい
                       } deriving (Show, Generic, ToJSON, FromJSON)

type OwnerName = T.Text -- この辺もnewtypeとかでいい感じに汎化したい
type RepoName = T.Text
type IssueNumber = Int

editIssueHttpRequest :: OwnerName -> RepoName -> IssueNumber -> ReqBody -> Auth -> HTTP.Request
editIssueHttpRequest ownerName repoName issueNumber body auth =
    withAuth auth . withBody body . mkHttpRequest $ req
  where
    req = Request { reqPath = T.intercalate "/" ["repos", ownerName, repoName, "issues", T.pack $ show issueNumber]
                  , reqMethod = PATCH
                  }

editIssue :: OwnerName -> RepoName -> IssueNumber -> ReqBody -> Auth -> IO (Either Error Issue)
editIssue ownerName repoName issueNumber body auth =
    getResponseBodyEither <$> sendRequest' httpReq
  where
    httpReq = editIssueHttpRequest ownerName repoName issueNumber body auth