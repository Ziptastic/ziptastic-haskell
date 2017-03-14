{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , TypeOperators
#-}

-- | This module provides a complete and type-safe API specification for
-- Ziptastic's forward and reverse geocoding API using Servant
-- (<https://www.getziptastic.com/>).
--
-- To use this specification in your application, try the @ziptastic-client@ package.
module Ziptastic.Core
  ( Api
  , ApiKey(..)
  , ForApi(..)
  , LocaleCoords(..)
  , LocaleInfo(..)
  , baseUrlHost
  , baseUrlIsHttps
  , baseUrlPath
  , baseUrlPort
  ) where

import           Control.Monad (when)
import           Data.ISO3166_CountryCodes (CountryCode)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import           Data.Aeson ((.:), (.:?))
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Zones.All (TZLabel, fromTZName)
import           GHC.Generics (Generic)
import           Servant.API (Capture, Get, Header, JSON, (:>), (:<|>))
import           Text.Read (readMaybe)
import           Web.HttpApiData (ToHttpApiData, toUrlPiece)


data LocaleCoords = LocaleCoords
  { coordsLatitude  :: Double
  , coordsLongitude :: Double
  , coordsGeohash   :: Text    -- ^ Will never be empty.
  } deriving (Eq, Generic, Show)

data LocaleInfo = LocaleInfo
  { localeCity         :: Maybe Text  -- ^ If given, text will never be empty.
  , localeCoords       :: Maybe LocaleCoords
  , localeCountry      :: CountryCode
  , localeCounty       :: Maybe Text -- ^ If given, text will never be empty.
  , localeRegionFull   :: Maybe Text -- ^ If given, text will never be empty.
  , localeRegionAbbrev :: Maybe Text -- ^ If given, text will never be empty.
  , localePostalCode   :: Text       -- ^ Will never be empty.
  , localeTimeZone     :: Maybe TZLabel
  } deriving (Eq, Generic, Show)

instance Json.FromJSON LocaleInfo where
  parseJSON (Json.Object v) = do
    maybeLat     <- v .:? "latitude"
    maybeLong    <- v .:? "longitude"
    maybeGeohash <- v `optionalStr` "geohash"

    countryCodeStr <- v .: "country"
    maybeTzText <- v `optionalStr` "timezone"

    postalCode <- v .: "postal_code"
    when (T.null postalCode) $ fail "Invalid postal code"

    let
      coords = case (maybeLat, maybeLong, maybeGeohash) of
        (Just lat, Just long, Just geohash) -> Just (LocaleCoords lat long geohash)
        _ -> Nothing

      tzParser :: Text -> Json.Parser TZLabel
      tzParser tz = maybe (fail $ "Unrecognized time zone: " ++ T.unpack tz) pure (fromTZName $ encodeUtf8 tz)

    LocaleInfo
      <$> v `optionalStr` "city"
      <*> pure coords
      <*> maybe (fail $ "Unrecognized country code: " ++ countryCodeStr) pure (readMaybe countryCodeStr)
      <*> v `optionalStr` "county"
      <*> v `optionalStr` "state"
      <*> v `optionalStr` "state_short"
      <*> pure postalCode
      <*> maybe (pure Nothing) (fmap Just . tzParser) maybeTzText

    where
      optionalStr v' key = do
        val <- v' .:? key
        pure $ case val of
          Just x -> if T.null x then Nothing else Just x
          Nothing -> Nothing

  parseJSON x = Json.typeMismatch "LocaleInfo" x

newtype ApiKey = ApiKey { getApiKey :: Text } deriving (Eq, Generic, IsString, Show, ToHttpApiData)

-- | A generic wrapper for giving external data types instances for our uses.
newtype ForApi a = ForApi a

instance ToHttpApiData (ForApi CountryCode) where
  toUrlPiece (ForApi countryCode) = T.pack (show countryCode)

type Api = "v3" :> Header "x-key" ApiKey :> ApiEndpoints
type ApiEndpoints = ForwardGeocodingApi :<|> ReverseGeocodingApi

type LocaleInfoResponse = Get '[JSON] [LocaleInfo]

type ForwardGeocodingApi
  =  Capture "country-code" (ForApi CountryCode)
  :> Capture "postal-code" Text
  :> LocaleInfoResponse

type ReverseGeocodingApi
  =  "reverse"
  :> Capture "latitude"  Double
  :> Capture "longitude" Double
  :>
    (    Capture "radius-in-meters" Int :> LocaleInfoResponse
    :<|> LocaleInfoResponse  -- use default radius
    )


-- API URL components
baseUrlIsHttps :: Bool
baseUrlIsHttps = True

baseUrlHost :: String
baseUrlHost = "zip.getziptastic.com"

baseUrlPort :: Int
baseUrlPort = 443

baseUrlPath :: String
baseUrlPath = ""
