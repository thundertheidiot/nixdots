{-# LANGUAGE LambdaCase #-}

module Main where

import Distribution.Simple.Utils
import System.Environment

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
        _ -> putStrLn "ligma"

    showKeys "sops/wireguard"

    putStrLn $ "Emacs: " ++ show emacs
