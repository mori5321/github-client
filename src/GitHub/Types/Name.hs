module GitHub.Types.Name where


import           Data.String                    ( IsString
                                                , fromString
                                                )
import qualified Data.Text                     as T
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , toJSON
                                                , parseJSON
                                                )

newtype Name entity = N T.Text
    deriving (Eq, Ord, Show)

mkName :: proxy entity -> T.Text -> Name entity
mkName _ = N

untagName :: Name entity -> T.Text
untagName (N name) = name

instance FromJSON (Name entity) where
    parseJSON = fmap N . parseJSON

instance IsString (Name entity) where
    fromString = N . fromString
