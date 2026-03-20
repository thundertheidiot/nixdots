{-# LANGUAGE LambdaCase #-}

module Cli where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Nix.Build
import Wireguard

import Data.Text (pack)

data WireguardAction = WgAdd String | WgRemove String
    deriving (Show)

toWgAction :: WireguardAction -> IO (WireguardKeys -> WireguardKeys)
toWgAction (WgAdd s) = addKey <$> (newWgKey $ pack s)
toWgAction (WgRemove s) = pure . removeKey $ pack s

type WgParse a = Either a String

add s = s == "add" || s == "a"
del s = s == "del" || s == "d" || s == "r"

parseWgArg :: [String] -> WgParse (WireguardAction, [String])
parseWgArg (act : name : rest)
    | add act = Left (WgAdd name, rest)
    | del act = Left (WgRemove name, rest)
parseWgArg (act : [])
    | add act || del act = Right "expected name"
parseWgArg _ = Right "invalid keyword"

parseWgArgs' :: [String] -> [WireguardAction] -> WgParse [WireguardAction]
parseWgArgs' [] actions = Left actions
parseWgArgs' args actions =
    case parseWgArg args of
        Left (action, rest) -> parseWgArgs' rest (actions ++ [action])
        Right error -> Right error

parseWgArgs :: [String] -> WgParse [WireguardAction]
parseWgArgs = flip parseWgArgs' $ []

wgAct :: [WireguardAction] -> IO (Maybe Bool)
wgAct actions =
    -- flip because [add remove] = id . add . remove => id (add (remove))
    modifyKeys "sops/wireguard" =<< foldl' (flip (.)) id <$> mapM toWgAction actions

wgArgs :: [String] -> IO ()
wgArgs args =
    case args of
        [] -> liftIO $ putStrLn "no arguments given"
        ("show" : []) -> liftIO $ showKeys "sops/wireguard"
        args -> liftIO $ do
            Just actions <- case parseWgArgs args of
                Left actions -> return . Just $ actions
                Right error -> do
                    putStrLn error
                    return Nothing
            Just status <- wgAct actions
            putStrLn . show $ status
