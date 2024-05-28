{-
All numbers in this module are taken from the set of test personal numbers
released by skatteverket under CC0:
https://www7.skatteverket.se/portal/apier-och-oppna-data/utvecklarportalen/oppetdata/Test%C2%AD%C2%ADpersonnummer
-}
module Text.NationalIdentifier.Sweden.PersonalNumberSpec where

import Data.Either
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Text.NationalIdentifier.Sweden.PersonalNumber

test19thCentury, test20thCentury, testCentenarian :: Text
test19thCentury = "193701139051"
test20thCentury = "201411032392"
testCentenarian = "189605249813"

spec :: Spec
spec = describe "mkPersonalNumberSE" $ do
    it "Can parse 10 digits" $ do
        toTwelveDigits <$> mkPersonalNumberSE "3701139051" `shouldBe` Right test19thCentury
    it "Can parse 10 digits with -" $ do
        toTwelveDigits <$> mkPersonalNumberSE "370113-9051" `shouldBe` Right test19thCentury
    it "Can parse 12 digits" $ do
        toTwelveDigits <$> mkPersonalNumberSE "193701139051" `shouldBe` Right test19thCentury
    it "Can parse 12 digits with -" $ do
        toTwelveDigits <$> mkPersonalNumberSE "19370113-9051" `shouldBe` Right test19thCentury
    it "Can parse centenarian (10 digits with +)" $ do
        toTwelveDigits <$> mkPersonalNumberSE "960524+9813" `shouldBe` Right testCentenarian
    it "Can parse person born in 20th century (10 digits)" $ do
        toTwelveDigits <$> mkPersonalNumberSE "1411032392" `shouldBe` Right test20thCentury
    it "Can parse list of know personal numbers" $ do
        lefts (fmap mkPersonalNumberSE samples) `shouldBe` []
    it "Can parse list of know personal numbers from 10 digits" $ do
        map toTwelveDigits (rights (map (mkPersonalNumberSE . Text.drop 2) samples)) `shouldBe` samples
    it "Fails to parse list of known invalid numbers" $ do
        rights (fmap mkPersonalNumberSE invalidSamples) `shouldBe` []

    it "Can parse personal numbers with surrounding whitespace" $ do
        toTwelveDigits <$> mkPersonalNumberSE "3701139051  " `shouldBe` Right test19thCentury
        toTwelveDigits <$> mkPersonalNumberSE "  370113-9051" `shouldBe` Right test19thCentury

invalidSamples :: [Text]
invalidSamples =
    [ "4209186975"
    , "501204213-0"
    , "197906052382"
    ]

samples :: [Text]
samples =
    [ "194209186974"
    , "194209257486"
    , "194209309097"
    , "195004112354"
    , "195011282182"
    , "195012042130"
    , "195910162642"
    , "195910192631"
    , "195910312577"
    , "197608282385"
    , "197608292392"
    , "197608292384"
    , "197906052399"
    , "197906052381"
    , "197906062380"
    , "200002292381"
    , "200003017282"
    , "200003015542"
    , "201307052389"
    , "201307062396"
    , "201307062388"
    ]
