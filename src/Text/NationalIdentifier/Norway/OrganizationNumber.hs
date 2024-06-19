module Text.NationalIdentifier.Norway.OrganizationNumber
    ( OrganizationNumberNO
    , toNineDigits
    , mkOrganizationNumberNO
    )
where

import Control.Monad
import Data.Aeson
import Data.Char (digitToInt, intToDigit)
import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

newtype OrganizationNumberNO = OrganizationNumberNO {toNineDigits :: Text}
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON)

instance FromJSON OrganizationNumberNO where
    parseJSON = withText "OrganizationNumberNO" $ pure . OrganizationNumberNO

-- See https://www.brreg.no/om-oss/registrene-vare/om-enhetsregisteret/organisasjonsnummeret/
mkOrganizationNumberNO :: Text -> Either String OrganizationNumberNO
mkOrganizationNumberNO raw = do
    let digits = map digitToInt . filter Char.isDigit $ T.unpack raw
    unless (length digits == 9) $ do
        Left $ "Invalid Norwegian organization number: " <> T.unpack raw
    let noncontrol = take 8 digits
    let control = (11 - sum (zipWith (*) noncontrol [3, 2, 7, 6, 5, 4, 3, 2])) `mod` 11
    -- Note that if `control` is 10 the check will always fail as expected.
    unless (noncontrol <> [control] == digits) $ do
        Left $ "Invalid Norwegian organization number (invalid control digit): " <> T.unpack raw
    pure . OrganizationNumberNO . T.pack $ map intToDigit digits
