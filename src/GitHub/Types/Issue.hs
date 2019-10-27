{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module GitHub.Types.Issue
    ( Issue(..)
    , Filter(..)
    , State(..)
    , Label(..)
    , Sort(..)
    , Direction(..)
    , Since
    , QueryItem(..)
    )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics
import qualified Data.ByteString.Char8 as S8
import Data.List
import Data.Time
import Data.Time.ISO8601

import GitHub.Request (  IsHTTPQueryItem(..) )


data Issue = Issue { id :: Integer
                   } deriving (Show, Generic, FromJSON, ToJSON)

data Filter = Assigned | Created | Mentioned | Subscribed | All
instance IsHTTPQueryItem Filter where
    toHTTPQueryItem Assigned   = ("filter", Just "assigned")
    toHTTPQueryItem Created    = ("filter", Just "created")
    toHTTPQueryItem Mentioned  = ("filter", Just "mentioned")
    toHTTPQueryItem Subscribed = ("filter", Just "subscribed")
    toHTTPQueryItem All        = ("filter", Just "all")

data State = Open | Closed | AllState
instance IsHTTPQueryItem State where
    toHTTPQueryItem Open     = ("state", Just "open")
    toHTTPQueryItem Closed   = ("state", Just "closed")
    toHTTPQueryItem AllState = ("state", Just "all")

newtype Label = Label S8.ByteString
type Labels = [Label]

unwrapLabel :: Label -> S8.ByteString
unwrapLabel (Label bs) = bs

instance IsHTTPQueryItem Labels where
    toHTTPQueryItem labels = ("labels", Just $ S8.intercalate "," $ map unwrapLabel labels)

data Sort = SortByCreated | SortByUpdated | SortByComments
instance IsHTTPQueryItem Sort where
    toHTTPQueryItem SortByCreated  = ("sort", Just "created")
    toHTTPQueryItem SortByUpdated  = ("sort", Just "updated")
    toHTTPQueryItem SortByComments = ("sort", Just "comments")

data Direction = Desc | Asc
instance IsHTTPQueryItem Direction where
    toHTTPQueryItem Desc = ("direction", Just "desc")
    toHTTPQueryItem Asc = ("direction", Just "asc")

type Since = UTCTime
instance IsHTTPQueryItem Since where
    toHTTPQueryItem since = ("since", Just $ S8.pack $ formatISO8601 since)

data QueryItem = forall a . IsHTTPQueryItem a => QueryItem a
instance IsHTTPQueryItem QueryItem where
    toHTTPQueryItem (QueryItem qi) = toHTTPQueryItem qi