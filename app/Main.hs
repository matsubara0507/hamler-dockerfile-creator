module Main where

import qualified Data.Text.Lazy.IO         as L
import           Dockerfile.Creator.Hamler (Config (..), buildDockerfile)
import           System.Console.GetOpt
import           System.Environment        (getArgs)

data Flag 
  = HamdlerVersion (Maybe String)
  | OTPVersion (Maybe String)
  deriving (Show, Eq)

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute opts args of
    (flags, _, []) -> do
      r <- buildDockerfile (buildConfig flags)
      case r of
        Right df -> 
          L.putStr df
        Left err ->
          fail err
    (_, _, errs) -> 
      fail (concat errs ++ usageInfo header opts)
  where
    header = 
      "Usage: hdc [OPTION...]"
    opts = 
      [ Option [] ["hamler"] (OptArg HamdlerVersion "VERSION") "Select Hamler version to build Dockerfile"
      , Option [] ["otp"] (OptArg OTPVersion "VERSION") "Select Erlang/OTP version to build Dockerfile"
      ]

buildConfig :: [Flag] -> Config
buildConfig = go (Config Nothing Nothing)
  where
    go config []                      = config
    go config (HamdlerVersion v : fs) = go (config { versionHamler = v }) fs
    go config (OTPVersion v : fs)     = go (config { versionOTP = v }) fs
