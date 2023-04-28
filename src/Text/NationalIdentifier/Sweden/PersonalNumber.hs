module Text.NationalIdentifier.Sweden.PersonalNumber
    ( PersonalNumberSE
    , toTwelveDigits
    , mkPersonalNumberSE
    ) where

import Control.Applicative
import Control.Monad (unless)
import Data.Aeson
import Data.Bifunctor (first)
import Data.Char qualified as Char
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Void
import GHC.Generics (Generic)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Printf (printf)

newtype PersonalNumberSE = PersonalNumberSE {toTwelveDigits :: Text}
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype ToJSON

instance FromJSON PersonalNumberSE where
  parseJSON = withText "PersonalNumberSE" $ either fail pure . mkPersonalNumberSE

mkPersonalNumberSE :: Text -> Either String PersonalNumberSE
mkPersonalNumberSE = runP parsePersonalNumberSE

runP :: P.Parsec Void Text a -> Text -> Either String a
runP p = first P.errorBundlePretty . P.parse p ""

parseDigits :: Int -> P.Parsec Void Text Int
parseDigits n = read <$> P.count n C.digitChar

parsePersonalNumberSE :: P.Parsec Void Text PersonalNumberSE
parsePersonalNumberSE = P.try year4 <|> year2
  where
    year4 = do
        year <- parseDigits 4
        month <- parseDigits 2
        dayOfMonth <- parseDigits 2
        birthday <- case fromGregorianValid (fromIntegral year) month dayOfMonth of
            Nothing -> fail "Invalid birthday"
            Just b -> pure b
        _ <-
            P.choice
                [ C.char '-'
                , pure '?'
                ]
        rest <- parseDigits 4
        P.eof
        let pNo =
                T.filter Char.isDigit (T.pack $ show birthday)
                    <> T.pack (printf "%04d" rest)
        either fail pure $ validControlDigit pNo
    year2 = do
        year' <- parseDigits 2
        month <- parseDigits 2
        dayOfMonth <- parseDigits 2
        c <-
            P.choice
                [ C.char '-'
                , C.char '+' -- technically + means person is >100 years
                , pure '?'
                ]
        let year = case c of
                '+' -> case 1900 + year' of
                    y
                        | 2023 - y < 100 -> y - 100
                        | otherwise -> y
                _
                    | year' > 22 -> 1900 + year'
                    | otherwise -> 2000 + year'
        birthday <- case fromGregorianValid (fromIntegral year) month dayOfMonth of
            Nothing -> fail "Invalid birthday"
            Just b -> pure b
        rest <- parseDigits 4
        P.eof
        let pNo =
                T.filter Char.isDigit (T.pack $ show birthday)
                    <> T.pack (printf "%04d" rest)
        either fail pure $ validControlDigit pNo

-- | Accepts 12 digit personal number with only digits iff control digit is correct
validControlDigit :: Text -> Either String PersonalNumberSE
validControlDigit raw = do
    unless
        (all Char.isDigit t && length t == 12)
        (Left $ "Expecting 12 digits, got: " <> t)
    let significantDigits = fmap Char.digitToInt . take 9 $ drop 2 t
        controlDigit = Char.digitToInt $ L.last t
    if (10 - digitSum significantDigits) `mod` 10 == controlDigit
        then Right $ PersonalNumberSE raw
        else Left "Invalid control digit"
  where
    t = T.unpack raw

    digitSum :: [Int] -> Int
    digitSum =
        sum
            . fmap Char.digitToInt
            . foldMap show
            . zipWith (*) (L.cycle [2, 1])
