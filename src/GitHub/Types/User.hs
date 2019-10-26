{-# LANGUAGE DeriveGeneric #-}

module GitHub.Types.User
    ( User(..)
    )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text as T
import GHC.Generics

data User = User { id :: Integer
                 , login :: T.Text
                 } deriving (Show, Generic)
instance FromJSON User
instance ToJSON User
