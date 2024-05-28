module Text.NationalIdentifier.Norway.PersonalNumber
    ( PersonalNumberNO
    , toElevenDigits
    , mkPersonalNumberNO
    ) where

import Data.Aeson
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Void
import GHC.Generics (Generic)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Printf (printf)

newtype PersonalNumberNO = PersonalNumberNO {toElevenDigits :: Text}
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype ToJSON

instance FromJSON PersonalNumberNO where
  parseJSON = withText "PersonalNumberNO" $ either fail pure . mkPersonalNumberNO

mkPersonalNumberNO :: Text -> Either String PersonalNumberNO
mkPersonalNumberNO = runP parsePersonalNumberNO . T.strip

runP :: P.Parsec Void Text a -> Text -> Either String a
runP p = first P.errorBundlePretty . P.parse p ""

parseDigits :: Int -> P.Parsec Void Text Int
parseDigits n = read <$> P.count n C.digitChar

parsePersonalNumberNO :: P.Parsec Void Text PersonalNumberNO
parsePersonalNumberNO = do
    dayOfMonth <- parseDigits 2
    month <- parseDigits 2
    year' <- parseDigits 2
    lopNummer <- parseDigits 3
    control <- parseDigits 2
    P.eof

    let year = year' + if lopNummer < 500 then 1900 else 2000
    _birthday <- case fromGregorianValid (fromIntegral year) month dayOfMonth of
        Nothing -> fail "Invalid birthday"
        Just b -> pure b
    pure . PersonalNumberNO $
        T.pack (printf "%02d" dayOfMonth)
            <> T.pack (printf "%02d" month)
            <> T.pack (printf "%02d" year')
            <> T.pack (printf "%03d" lopNummer)
            <> T.pack (printf "%02d" control)
