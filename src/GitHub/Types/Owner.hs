{-# LANGUAGE DeriveGeneric #-}

module GitHub.Types.Owner
    ( Owner
    )
where

import           GHC.Generics                   ( Generic )
import           GitHub.Types.User              ( User )

newtype Owner = Owner User
    deriving (Show, Generic)
