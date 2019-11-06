{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module GitHub.Requests.Issues.Get
  ( getIssue
  , getIssueHttpRequest
  )
where

import GHC.Generics
import qualified Data.Text as T
import qualified Network.HTTP.Simple as HTTP

import GitHub.Types.User ( User(..) )
import GitHub.Types.Issue ( Issue(..) )
import GitHub.Query ( toHTTPQueryItem, QueryItem )
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

type OwnerName = T.Text
type RepoName = T.Text
type IssueNumber = Int

getIssueHttpRequest :: OwnerName ->
                       RepoName ->
                       IssueNumber ->
                       Auth ->
                       HTTP.Request
getIssueHttpRequest ownerName repoName issueNumber auth =
    withAuth auth . mkHttpRequest $ req
  where
    req = Request { reqPath = T.intercalate "/" ["repos", ownerName, repoName, "issues", T.pack . show $ issueNumber]
                  , reqMethod = GET
                  }

getIssue :: OwnerName ->
            RepoName ->
            IssueNumber ->
            Auth ->
            IO (Either Error Issue)
getIssue ownerName repoName issueNumber auth =
    getResponseBodyEither <$> sendRequest' httpReq
  where
    httpReq = getIssueHttpRequest ownerName repoName issueNumber auth