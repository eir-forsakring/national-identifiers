module Text.NationalIdentifier.Sweden.OrganizationNumber
    ( OrganizationNumberSE
    , toTenDigits
    , mkOrganizationNumberSE
    )
where

import Control.Monad (unless)
import Data.Aeson
import Data.Bifunctor (first)
import Data.Char qualified as Char
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import GHC.Generics (Generic)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Printf (printf)

newtype OrganizationNumberSE = OrganizationNumberSE {toTenDigits :: Text}
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype ToJSON

instance FromJSON OrganizationNumberSE where
  parseJSON = withText "OrganizationNumberSE" $ either fail pure . mkOrganizationNumberSE

mkOrganizationNumberSE :: Text -> Either String OrganizationNumberSE
mkOrganizationNumberSE = runP parseOrganizationNumberSE . T.strip

runP :: P.Parsec Void Text a -> Text -> Either String a
runP p = first P.errorBundlePretty . P.parse p ""

parseDigits :: Int -> P.Parsec Void Text Int
parseDigits n = read <$> P.count n C.digitChar

parseOrganizationNumberSE :: P.Parsec Void Text OrganizationNumberSE
parseOrganizationNumberSE = do
    initial <- parseDigits 6
    _ <-
        P.choice
            [ C.char '-'
            , pure '?'
            ]
    rest <- parseDigits 4
    P.eof
    let orgNo = T.pack (printf "%06d" initial) <> T.pack (printf "%04d" rest)
    either fail pure $ validControlDigit orgNo

validControlDigit :: Text -> Either String OrganizationNumberSE
validControlDigit raw = do
    unless
        (all Char.isDigit t && length t == 10)
        (Left $ "Expecting 10 digits, got: " <> t)
    let significantDigits = Char.digitToInt <$> take 9 t
        controlDigit = Char.digitToInt $ L.last t
    if (10 - digitSum significantDigits) `mod` 10 == controlDigit
        then Right $ OrganizationNumberSE raw
        else Left "Invalid control digit"
  where
    t = T.unpack raw
    digitSum :: [Int] -> Int
    digitSum =
        sum
            . fmap Char.digitToInt
            . foldMap show
            . zipWith (*) (L.cycle [2, 1])
