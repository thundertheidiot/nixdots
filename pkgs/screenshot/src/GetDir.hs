{-# LANGUAGE LambdaCase #-}
module GetDir where

import Data.List (isPrefixOf, intercalate)
import Data.Maybe (mapMaybe)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

xdgConfigHome :: IO (Maybe String)
xdgConfigHome = do
  xdgConfigHome <- lookupEnv "XDG_CONFIG_HOME"
  case xdgConfigHome of
    Just e -> return (Just e)
    Nothing -> do
      home <- lookupEnv "HOME"
      case home of
        Just homePath -> return (Just (homePath </> "/.config/"))
        Nothing -> return Nothing

xdgDirsFile :: IO (Maybe String)
xdgDirsFile = do
  dotConfig <- xdgConfigHome
  case dotConfig of
    Just dir -> return (Just (dir </> "user-dirs.dirs"))
    Nothing -> return Nothing

parseXdgDirs :: String -> [(String, String)]
parseXdgDirs content =
  mapMaybe parseLine (lines content)
  where
    parseLine :: String -> Maybe (String, String)
    parseLine line = case break (=='=') line of
      (key, '=':value) -> Just (key, removeQuotes value)
      _ -> Nothing

    removeQuotes :: String -> String
    removeQuotes str
      | isPrefixOf "\"" str && isPrefixOf "\"" (reverse str) = init (tail str)
      | otherwise = str

getPicturesDir :: IO (Maybe String)
getPicturesDir =
  xdgDirsFile >>= \case
    Just f -> do
      cont <- readFile f
      let pictures = lookup "XDG_PICTURES_DIR" $ parseXdgDirs cont
        in case pictures of
             Just dir -> return (Just dir)
             Nothing -> assumedPicturesDir
    Nothing -> assumedPicturesDir
  where
    assumedPicturesDir :: IO (Maybe String)
    assumedPicturesDir =
      lookupEnv "HOME" >>= \case
      Just env -> return (Just (env </> "Pictures/"))
      Nothing -> return Nothing

dir :: IO (Maybe String)
dir =
  getPicturesDir >>= \case
  Just dir -> return (Just (dir </> "screenshots/"))
  Nothing -> return Nothing
