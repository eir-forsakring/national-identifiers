-- All numbers in this file are randomly generated.
module Text.NationalIdentifier.Norway.PersonalNumberSpec where

import Data.Either
import Data.Text (Text)
import Test.Hspec
import Text.NationalIdentifier.Norway.PersonalNumber

spec :: Spec
spec = describe "mkPersonalNumberNO" $ do
    it "Can parse 11 digits" $ do
        toElevenDigits <$> mkPersonalNumberNO "09043949095" `shouldBe` Right "09043949095"
    it "Can parse list of know valid personal numbers" $ do
        lefts (fmap mkPersonalNumberNO samples) `shouldBe` []
    it "Fails to parse list of known invalid numbers" $ do
        rights (fmap mkPersonalNumberNO invalidSamples) `shouldBe` []

invalidSamples :: [Text]
invalidSamples =
    [ "37052818928"
    , "170528189231"
    , "1705281892"
    ]

samples :: [Text]
samples =
    [ "25122241222"
    , "30015249389"
    , "09043949095"
    , "28109326633"
    , "17061099905"
    , "09105305041"
    , "12111089955"
    , "03128802281"
    , "23078719184"
    , "05128647408"
    , "04110420094"
    , "02098908016"
    , "26097320658"
    , "20023716381"
    , "04104610329"
    , "29100079141"
    , "24071372827"
    , "20101313000"
    , "17052818923"
    , "18048845963"
    , "07077104881"
    , "01111241720"
    , "01082077134"
    , "16071221591"
    , "06054125192"
    , "14036123150"
    , "31078020205"
    , "15099002708"
    , "01119348907"
    , "15121569598"
    , "10037924488"
    , "01070473735"
    , "10105707822"
    , "01110932711"
    , "15026219504"
    , "03051089188"
    , "29096737651"
    , "30035141657"
    , "19012160561"
    , "19051455218"
    , "19038306408"
    , "17042376833"
    , "15114327236"
    , "10042304072"
    , "11020083907"
    , "26056817831"
    , "09030585248"
    , "06011126527"
    , "25061287895"
    , "24097423071"
    , "23106310335"
    , "14068304734"
    , "02015208409"
    , "26021104875"
    ]
