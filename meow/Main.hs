{-# LANGUAGE LambdaCase #-}

module Main where

import Distribution.Simple.Utils
import System.Environment

import Cli
import Data.List
import Nix.Build
import Wireguard

inEmacs :: IO Bool
inEmacs =
    lookupEnv "INSIDE_EMACS" >>= \case
        Just "vterm" -> return False
        Just _ -> return True
        Nothing -> return False

-- safeTail :: [a] -> Maybe [a]
-- safeTail = fmap snd . uncons

main :: IO ()
main = do
    emacs <- inEmacs
    args <- getArgs

    case safeHead args of
        Just "wg" -> do
            wgArgs $ safeTail args
        Just "build" -> do
            Just out <- nixBuild [".#nixosConfigurations." ++ (head . tail $ args) ++ ".config.system.build.toplevel"] emacs
            putStrLn out
    --   putStrLn "build"
    -- _ -> putStrLn "ligma"

    putStrLn $ "Emacs: " ++ show emacs
