module Text.NationalIdentifier.Norway.OrganizationNumber
    ( OrganizationNumberNO
    , toSevenDigits
    , mkOrganizationNumberNO
    )
where

import Data.Aeson
import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

newtype OrganizationNumberNO = OrganizationNumberNO {toSevenDigits :: Text}
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype ToJSON

instance FromJSON OrganizationNumberNO where
  parseJSON = withText "OrganizationNumberNO" $ either fail pure . mkOrganizationNumberNO

mkOrganizationNumberNO :: Text -> Either String OrganizationNumberNO
mkOrganizationNumberNO t = case T.filter Char.isDigit t of
    orgNo | T.length orgNo > 7 -> Right $ OrganizationNumberNO orgNo
    _ -> Left $ "Invalid organization number: " <> T.unpack t
