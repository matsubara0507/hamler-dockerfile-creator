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

data Config = Config
    { versionHamler :: Maybe String
    , versionOTP    :: Maybe String
    }

-- | return download url for deb file
fetchHamlerDebUrl :: MonadIO m => Maybe String -> m (Maybe String)
fetchHamlerDebUrl version = do
  release <- fetchRelease "hamler-lang" "hamler" version
  pure $ do
    assets <- GitHub.releaseAssets <$> release
    asset <- List.find (isHamlerDeb . GitHub.releaseAssetName) assets
    pure $ Text.unpack (GitHub.releaseAssetBrowserDownloadUrl asset)
  where
    isHamlerDeb txt = "hamler_" `Text.isPrefixOf` txt && ".deb" `Text.isSuffixOf` txt

-- | return version
validateVersionOTP :: MonadIO m => Maybe String -> m (Maybe String)
validateVersionOTP version = do
  release <- fetchRelease "erlang" "otp" $ fmap (tagPrefix <>) version
  pure $ fmap (Text.unpack . Text.drop (length tagPrefix) . GitHub.releaseTagName) release
  where
    tagPrefix = "OTP-"

fetchRelease
  :: MonadIO m
  => GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> Maybe String
  -> m (Maybe GitHub.Release)
fetchRelease owner repo tag = do
  r <- liftIO $ case tag of
    Nothing ->
      github' $ GitHub.latestReleaseR owner repo
    Just t ->
      github' $ GitHub.releaseByTagNameR owner repo (fromString t)
  case r of
    Left _ ->
      pure Nothing
    Right release ->
      pure (Just release)

buildDockerfile :: MonadIO m => Config -> m (Either String L.Text)
buildDockerfile Config{..} = do
  r1 <- fetchHamlerDebUrl versionHamler
  r2 <- validateVersionOTP versionOTP
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
