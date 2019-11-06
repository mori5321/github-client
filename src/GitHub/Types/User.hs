{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHub.Types.User
    ( User(..)
    , Since(..)
    )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text as T
import GHC.Generics
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8

import GitHub.Query ( IsHTTPQueryItem(..), QueryItem )

data User = User { id :: Integer
                 , login :: T.Text
                 , avatar_url :: T.Text
                 } deriving (Show, Generic)
instance FromJSON User
instance ToJSON User

newtype Since = Since Int deriving Show
instance IsHTTPQueryItem Since where
    toHTTPQueryItem (Since int) = ("since", Just $ S8.pack $ show int)