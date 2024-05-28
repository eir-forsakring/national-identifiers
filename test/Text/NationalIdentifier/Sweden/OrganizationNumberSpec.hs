module Text.NationalIdentifier.Sweden.OrganizationNumberSpec where

import Data.Either
import Data.Text (Text)
import Test.Hspec
import Text.NationalIdentifier.Sweden.OrganizationNumber

spec :: Spec
spec = describe "parseOrganizationNumberSE" $ do
    it "Can parse 10 digits" $ do
        toTenDigits <$> mkOrganizationNumberSE "5591660617" `shouldBe` Right "5591660617"
    it "Can parse 10 digits with -" $ do
        toTenDigits <$> mkOrganizationNumberSE "559166-0617" `shouldBe` Right "5591660617"
    it "Can parse list of know personal numbers" $ do
        lefts (fmap mkOrganizationNumberSE samples) `shouldBe` []

    it "Fails to parse list of known invalid numbers" $ do
        rights (fmap mkOrganizationNumberSE invalidSamples) `shouldBe` []
    
    it "Can parse numbers with surrounding whitespace" $ do
        toTenDigits <$> mkOrganizationNumberSE "5591660617  " `shouldBe` Right "5591660617"
        toTenDigits <$> mkOrganizationNumberSE "  559166-0617" `shouldBe` Right "5591660617"

invalidSamples :: [Text]
invalidSamples =
    [ "559193-0441"
    , "5591670617"
    , "5567-037485"
    ]

samples :: [Text]
samples =
    [ "559193-0440"
    , "559166-0617"
    , "5567037485"
    ]
