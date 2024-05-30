module Text.NationalIdentifier.Norway.OrganizationNumberSpec where

import Data.Either
import Data.Text (Text)
import Test.Hspec
import Text.NationalIdentifier.Norway.OrganizationNumber

spec :: Spec
spec = describe "parseOrganizationNumberNO" $ do
  it "Can parse 9 digits" $ do
    toNineDigits <$> mkOrganizationNumberNO "923609016" `shouldBe` Right "923609016"
  it "Can parse 9 digits with -" $ do
    toNineDigits <$> mkOrganizationNumberNO "92360-9016" `shouldBe` Right "923609016"
  it "Can parse list of know personal numbers" $ do
    lefts (fmap mkOrganizationNumberNO samples) `shouldBe` []

  it "Fails to parse list of known invalid numbers" $ do
    rights (fmap mkOrganizationNumberNO invalidSamples) `shouldBe` []

  it "Can parse despite surrounding whitespace" $ do
    toNineDigits <$> mkOrganizationNumberNO "923609016  " `shouldBe` Right "923609016"
    toNineDigits <$> mkOrganizationNumberNO "  92360-9016" `shouldBe` Right "923609016"

invalidSamples :: [Text]
invalidSamples =
  [ "1",
    "2",
    "929423640" -- control digit would be 10
  ]

samples :: [Text]
samples =
  [ "923609016",
    "914778271",
    "982463718",
    "886581432",
    "910747711"
  ]
