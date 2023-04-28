module Text.NationalIdentifier.Norway.OrganizationNumberSpec where

import Data.Either
import Data.Text (Text)
import Test.Hspec
import Text.NationalIdentifier.Norway.OrganizationNumber

spec :: Spec
spec = describe "parseOrganizationNumberNO" $ do
    it "Can parse 10 digits" $ do
        toSevenDigits <$> mkOrganizationNumberNO "923609016" `shouldBe` Right "923609016"
    it "Can parse 10 digits with -" $ do
        toSevenDigits <$> mkOrganizationNumberNO "92360-9016" `shouldBe` Right "923609016"
    it "Can parse list of know personal numbers" $ do
        lefts (fmap mkOrganizationNumberNO samples) `shouldBe` []

    it "Fails to parse list of known invalid numbers" $ do
        rights (fmap mkOrganizationNumberNO invalidSamples) `shouldBe` []

invalidSamples :: [Text]
invalidSamples =
    [ "1"
    , "2"
    ]

samples :: [Text]
samples =
    [ "923609016"
    , "914778271"
    , "982463718"
    , "886581432"
    , "910747711"
    ]
