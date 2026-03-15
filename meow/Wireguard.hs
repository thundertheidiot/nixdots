{-# LANGUAGE OverloadedStrings #-}

module Wireguard (
    Wireguard (public, private),
    WireguardKeys,
    newWgKey,
    readKeys,
    writeKeys,
    showKeys,
    addKey,
    removeKey,
    modifyKeys,
) where

import Sops

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.Text (Text, pack, strip, unpack)
import System.FilePath
import System.Process (callProcess, readProcess)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

toString :: B.ByteString -> String
toString = TL.unpack . TL.decodeUtf8

data Wireguard = Wireguard
    { public :: String
    , private :: String
    }
    deriving (Show)

type WireguardKeys = M.Map Text Wireguard

type PublicKeys = M.Map Text String
type PrivateKeys = M.Map Text String

deserializeWgKeys :: WireguardKeys -> (PublicKeys, PrivateKeys)
deserializeWgKeys keys = (M.map public keys, M.map private keys)

wg :: [String] -> String -> IO String
wg args = readProcess "wg" args

wgGenkey :: IO String
wgGenkey = wg ["genkey"] ""

wgPubkey :: String -> IO String
wgPubkey = wg ["pubkey"]

wgKeys :: IO Wireguard
wgKeys = do
    privkey <- wgGenkey
    pubkey <- wgPubkey privkey
    return
        Wireguard
            { public = pubkey
            , private = privkey
            }

newWgKey :: Text -> IO (Text, Wireguard)
newWgKey name = (name,) <$> wgKeys

getPublic :: FilePath -> IO (Maybe PublicKeys)
getPublic f = return =<< decode <$> B.readFile f

getPrivate :: FilePath -> IO (Maybe PrivateKeys)
getPrivate f = return =<< decodeStrictText . pack <$> sopsDecrypt f

readKeys :: FilePath -> IO (Maybe WireguardKeys)
readKeys path = do
    Just public <- getPublic $ path </> "public-keys.json"
    Just private <- getPrivate $ path </> "private-keys.json"

    return . Just $ M.intersectionWith toWg public private
  where
    toWg :: String -> String -> Wireguard
    toWg pub priv =
        Wireguard
            { public = pub
            , private = priv
            }

writeKeys :: FilePath -> WireguardKeys -> IO Bool
writeKeys p keys = do
    (public, private) <- return $ deserializeWgKeys keys

    B.writeFile (p </> "public-keys.json") $ encode public

    sopsEncrypt (p </> "private-keys.json") $ toString . encode $ private

showKeys :: FilePath -> IO ()
showKeys p = do
    Just keys <- readKeys p
    mapM_ (putStrLn . unpack) $ M.keys keys

addKey :: (Text, Wireguard) -> WireguardKeys -> WireguardKeys
addKey = flip M.union . uncurry M.singleton

removeKey :: Text -> WireguardKeys -> WireguardKeys
removeKey drop = M.filterWithKey (\k _ -> drop /= k)

modifyKeys :: FilePath -> (WireguardKeys -> WireguardKeys) -> IO (Maybe Bool)
modifyKeys p trans = do
    Just keys <- readKeys p
    write <- writeKeys p $ trans keys

    return $ Just write
