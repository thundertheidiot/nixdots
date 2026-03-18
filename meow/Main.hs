{-# LANGUAGE LambdaCase #-}

module Main where

import Distribution.Simple.Utils
import System.Environment

import Nix.Build
import Wireguard

inEmacs :: IO Bool
inEmacs =
    lookupEnv "INSIDE_EMACS" >>= \case
        Just "vterm" -> return False
        Just _ -> return True
        Nothing -> return False

main :: IO ()
main = do
    emacs <- inEmacs
    args <- getArgs

    case safeHead args of
        Just "wg" -> do
            showKeys "sops/wireguard"
        Just "build" -> do
            Just out <- nixBuild [".#nixosConfigurations." ++ (head . tail $ args) ++ ".config.system.build.toplevel"] emacs
            putStrLn out
        _ -> putStrLn "ligma"

    putStrLn $ "Emacs: " ++ show emacs
