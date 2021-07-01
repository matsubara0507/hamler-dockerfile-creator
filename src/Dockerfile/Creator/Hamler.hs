{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dockerfile.Creator.Hamler where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List              as List
import           Data.Maybe             (fromMaybe)
import           Data.String            (fromString)
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as L
import           GHC.Exts               (fromList)
import           GitHub                 (github')
import qualified GitHub
import           Language.Docker.EDSL

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Config = Config
    { versionHamler :: Maybe String
    , versionOTP    :: Maybe String
    }

-- | return download url for deb file
fetchHamlerDebUrl :: MonadIO m => Config -> m (Maybe String)
fetchHamlerDebUrl config = do
  r <- liftIO $ case versionHamler config of
    Nothing ->
      github' $ GitHub.latestReleaseR "hamler-lang" "hamler"
    Just v ->
      github' $ GitHub.releaseByTagNameR "hamler-lang" "hamler" (fromString v)
  case r of
    Left _ ->
      pure Nothing
    Right release -> pure $ do
      let assets = GitHub.releaseAssets release
      asset <- List.find (isHamlerDeb . GitHub.releaseAssetName) assets
      pure $ Text.unpack (GitHub.releaseAssetBrowserDownloadUrl asset)
  where
    isHamlerDeb txt = "hamler_" `Text.isPrefixOf` txt && ".deb" `Text.isSuffixOf` txt

-- | return version
validateVersionOTP :: MonadIO m => Config -> m (Maybe String)
validateVersionOTP config = liftIO $ do
  r <- liftIO $ case versionOTP config of
    Nothing ->
      github' $ GitHub.latestReleaseR "erlang" "otp"
    Just v ->
      github' $ GitHub.releaseByTagNameR "erlang" "otp" (tagPrefix <> fromString v)
  case r of
    Left _ ->
      pure Nothing
    Right release ->
      pure $ Just (Text.unpack $ Text.drop (Text.length tagPrefix) (GitHub.releaseTagName release))
  where
    tagPrefix = "OTP-"

buildDockerfile :: MonadIO m => Config -> m (Either String L.Text)
buildDockerfile config@Config{..} = do
  r1 <- liftIO $ fetchHamlerDebUrl config
  r2 <- liftIO $ validateVersionOTP config
  case (r1, r2) of
    (Nothing, _) ->
      pure (Left $ "hamler version is not found: " ++ fromMaybe "latest" versionHamler)

    (_, Nothing) ->
      pure (Left $ "Erlang/OTP version is not found: " ++ fromMaybe "latest" versionOTP)

    (Just url, Just v) -> pure $ Right $ toDockerfileText $ do
      from $ "erlang" `tagged` fromString v
      run $ fromString $ concat
        [ "apt-get update && apt-get install -y libtinfo5"
        , " && apt-get clean"
        , " && rm -rf /var/lib/apt/lists/*"
        ]
      run $ fromString $ concat
        [ "wget -q -O /tmp/hamler.deb " ++ url
        , " && dpkg -i /tmp/hamler.deb"
        , " && rm /tmp/hamler.deb"
        ]
      entrypoint $ fromList ["hamler"]
