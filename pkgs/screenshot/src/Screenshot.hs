{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Environment (getArgs)

import Options
import GetDir (dir)
import CliArgs (parseArgs)
import MenuArgs (callTofi, tofiToOpts)
import Grim ( grimToClipboard
            , grimToFile
            , GrimError ( Success
                        , GrimFailed
                        , WlCopyFailed
                        , DirectoryFailed ) )

import Data.Function ((&))
import Data.Time ( getCurrentTime
                 , formatTime
                 , defaultTimeLocale )

import System.FilePath ((</>))

timeString :: IO String
timeString = do
  formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" <$> getCurrentTime

createFilePath :: IO (Maybe FilePath)
createFilePath = do
  time <- timeString
  dir >>= \case
    Just d -> return $ Just (d </> (time ++ ".png"))
    Nothing -> return Nothing

takeScreenshot :: Options -> IO GrimError
takeScreenshot opts =
  case save opts of
    Clipboard -> grimToClipboard
    File (Just path) -> grimToFile path
    File Nothing -> createFilePath >>= \case
      Just p -> grimToFile p
      Nothing -> return DirectoryFailed

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> tofiToOpts >>= takeScreenshot >>= print
    _ -> parseArgs args & takeScreenshot >>= print
