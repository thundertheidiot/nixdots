{-# LANGUAGE LambdaCase #-}

module Main where

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

    putStrLn $ show args

    showKeys "../sops/wireguard"

    putStrLn $ "Emacs: " ++ show emacs
