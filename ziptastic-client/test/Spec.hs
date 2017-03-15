{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main (main) where

import           Prelude ()
import           Prelude.Compat
import           Data.ISO3166_CountryCodes (CountryCode(US))
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client (defaultManagerSettings)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Status (unauthorized401)
import           Servant.Client (ServantError(..))
import           Test.Hspec
import           Ziptastic.Client

apiKey :: ApiKey
apiKey = "fake-key"

main :: IO ()
main = do
  insecureManager <- newManager defaultManagerSettings
  secureManager   <- newManager tlsManagerSettings
  hspec $
    describe "http client machinery" $ do
      it "can make requests with secure manager" $ do
        response <- forwardGeocode apiKey secureManager US "48867"
        response `shouldSatisfy` \case
          Left (FailureResponse{responseStatus=status}) -> status == unauthorized401
          _ -> False

      it "can make requests with insecure manager" $ do
        response <- forwardGeocode apiKey insecureManager US "48867"
        response `shouldSatisfy` \case
          Left (FailureResponse{responseStatus=status}) -> status == unauthorized401
          _ -> False

