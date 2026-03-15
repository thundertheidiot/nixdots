{-# LANGUAGE DeriveGeneric #-}

module Sops (sopsEncrypt, sopsDecrypt) where

import Data.Aeson
import Data.Text (pack, strip)
import GHC.Generics
import System.Process (callProcess, readProcess)

sopsDecrypt :: FilePath -> IO String
sopsDecrypt f = readProcess "sops" ["-d", f] ""

data SopsFileStatus = SopsFileStatus
    { encrypted :: Bool
    }
    deriving (Show, Generic)

instance FromJSON SopsFileStatus

sopsEncrypt :: FilePath -> String -> IO Bool
sopsEncrypt f c = do
    writeFile f c
    callProcess "sops" ["--encrypt", "--in-place", f]

    Just (status :: SopsFileStatus) <- decodeStrictText . strip . pack <$> readProcess "sops" ["filestatus", f] ""

    return . encrypted $ status
