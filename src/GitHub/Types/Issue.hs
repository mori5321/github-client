{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module GitHub.Types.Issue
        ( Issue(..)
        , FilterQuery(..)
        , StateQuery(..)
        , LabelQuery(..)
        , SortQuery(..)
        , DirectionQuery(..)
        , SinceQuery
        )
where

import           GHC.Generics
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.ByteString.Char8         as S8
import qualified Data.Text                     as T
import           Data.List
import           Data.Time
import           Data.Time.ISO8601

import           GitHub.Query                   ( IsHTTPQueryItem(..)
                                                , QueryItem
                                                )
import           GitHub.Types.User              ( User )


data Issue = Issue { id :: Integer
                   , url :: T.Text
                   , title :: T.Text
                   , body :: T.Text
                   , created_at :: UTCTime
                   , updated_at :: UTCTime
                   , user :: User
                   , assignees :: [User]
                   , closed_at :: Maybe UTCTime
                   } deriving (Show, Generic, FromJSON, ToJSON)

data FilterQuery = Assigned | Created | Mentioned | Subscribed | All
instance IsHTTPQueryItem FilterQuery where
        toHTTPQueryItem Assigned   = ("filter", Just "assigned")
        toHTTPQueryItem Created    = ("filter", Just "created")
        toHTTPQueryItem Mentioned  = ("filter", Just "mentioned")
        toHTTPQueryItem Subscribed = ("filter", Just "subscribed")
        toHTTPQueryItem All        = ("filter", Just "all")

data StateQuery = Open | Closed | AllState
instance IsHTTPQueryItem StateQuery where
        toHTTPQueryItem Open     = ("state", Just "open")
        toHTTPQueryItem Closed   = ("state", Just "closed")
        toHTTPQueryItem AllState = ("state", Just "all")

newtype LabelQuery = LabelQuery T.Text
type LabelsQuery = [LabelQuery]

unwrapLabelQuery :: LabelQuery -> T.Text
unwrapLabelQuery (LabelQuery bs) = bs

instance IsHTTPQueryItem LabelsQuery where
        toHTTPQueryItem labels =
                ( "labels"
                , Just $ S8.pack $ T.unpack $ T.intercalate "," $ map
                        unwrapLabelQuery
                        labels
                )
    -- TODO: 文字周りはどこかで整理したい

data SortQuery = SortByCreated | SortByUpdated | SortByComments
instance IsHTTPQueryItem SortQuery where
        toHTTPQueryItem SortByCreated  = ("sort", Just "created")
        toHTTPQueryItem SortByUpdated  = ("sort", Just "updated")
        toHTTPQueryItem SortByComments = ("sort", Just "comments")

data DirectionQuery = Desc | Asc
instance IsHTTPQueryItem DirectionQuery where
        toHTTPQueryItem Desc = ("direction", Just "desc")
        toHTTPQueryItem Asc  = ("direction", Just "asc")

type SinceQuery = UTCTime
instance IsHTTPQueryItem SinceQuery where
        toHTTPQueryItem since = ("since", Just $ S8.pack $ formatISO8601 since)
