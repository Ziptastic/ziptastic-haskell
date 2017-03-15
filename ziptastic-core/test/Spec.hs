{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main (main) where

import           Control.Monad
import           Data.Aeson (decodeStrict')
import qualified Data.ISO3166_CountryCodes as CC
import           Data.Monoid
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Test.Hspec
import           Data.String.Here (here, iTrim)
import           Data.Time.Zones.All as TZ

import           Ziptastic.Core


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "JSON decoding LocaleInfo" $ do
  let parseTemplate :: Maybe (Text, Text) -> Maybe LocaleInfo
      parseTemplate = decodeStrict' . encodeUtf8 . exampleJsonTemplate

  it "accepts the standard example" $
     parseTemplate Nothing `shouldBe` Just exampleLocaleInfo

  it "nullifies coordinates if any related fields are null" $
    forM_ [ ("geohash", quoted ""), ("geohash", "null")
          , ("latitude", "null"), ("longitude", "null") ] $ \(field, val) ->
      parseTemplate (Just (field, val)) `shouldBe` Just exampleLocaleInfo{localeCoords = Nothing}

  it "accepts empty time zone" $
    forM_ [quoted "", "null"] $ \val ->
      parseTemplate (Just ("timezone", val))
        `shouldBe` Just exampleLocaleInfo{localeTimeZone = Nothing}

  it "does not accept invalid time zone" $
    parseTemplate (Just ("timezone", "blah")) `shouldBe` Nothing

  it "does not accept invalid country code" $
    parseTemplate (Just ("country", "zz")) `shouldBe` Nothing

  it "does not accept empty postal code" $
    parseTemplate (Just ("postal_code", quoted "")) `shouldBe` Nothing


exampleJsonTemplate :: Maybe (Text, Text) -> Text
exampleJsonTemplate fieldOverride = [iTrim|
    {
      ${f "city" $ quoted "Owosso"},
      ${f "geohash" $ quoted "dpshsfsytw8k"},
      ${f "country" $ quoted "US"},
      ${f "county" $ quoted "Shiawassee"},
      ${f "state" $ quoted "Michigan"},
      ${f "state_short" $ quoted "MI"},
      ${f "postal_code" $ quoted "48867"},
      ${f "latitude" "42.9934"},
      ${f "longitude" "-84.1595"},
      ${f "timezone" $ quoted "America/Detroit"}
    }
  |] :: Text
  where
    f :: Text -> Text -> Text
    f field defaultVal = quoted field <> ": " <> case fieldOverride of
      Nothing -> defaultVal
      Just (key, val) -> if field == key then val else defaultVal

quoted :: Text -> Text
quoted x = "\"" <> x <> "\""

exampleLocaleInfo :: LocaleInfo
exampleLocaleInfo = LocaleInfo
  { localeCity         = Just "Owosso"
  , localeCountry      = CC.US
  , localeCounty       = Just "Shiawassee"
  , localeRegionFull   = Just "Michigan"
  , localeRegionAbbrev = Just "MI"
  , localePostalCode   = "48867"
  , localeCoords       = Just LocaleCoords
      { coordsLatitude  = 42.9934
      , coordsLongitude = (-84.1595)
      , coordsGeohash   = "dpshsfsytw8k"
      }
  , localeTimeZone   = Just TZ.America__Detroit
  }
