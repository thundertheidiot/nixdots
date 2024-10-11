import Data.List (isPrefixOf, intercalate)
import Data.Maybe (mapMaybe)
import System.Process (readProcess)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

callTofi :: [String] -> IO String
callTofi args =
  readProcess "tofi" [] (intercalate "\n" args)

xdgConfigHome :: IO (Maybe String)
xdgConfigHome = do
  xdgConfigHome <- lookupEnv "XDG_CONFIG_HOME"
  case xdgConfigHome of
    Just e -> return (Just e)
    Nothing -> do
      home <- lookupEnv "HOME"
      case home of
        Just homePath -> return (Just (homePath </> "/.config"))
        Nothing -> return Nothing

xdgDirsFile :: IO (Maybe String)
xdgDirsFile = do
  dotConfig <- xdgConfigHome
  case dotConfig of
    Just dir -> return (Just (dir </> "user-dirs.dirs"))
    Nothing -> return Nothing

parseXdgDirs :: IO (Maybe String) -> IO (Maybe [(String, String)])
parseXdgDirs content = do
  cont <- content
  case cont of
    Just c -> return (Just (mapMaybe parseLine (lines c)))
    Nothing -> return Nothing
  where
    parseLine :: String -> Maybe (String, String)
    parseLine line = case break (=='=') line of
      (key, '=':value) -> Just (key, removeQuotes value)
      _ -> Nothing

    removeQuotes :: String -> String
    removeQuotes str
      | isPrefixOf "\"" str && isPrefixOf "\"" (reverse str) = init (tail str)
      | otherwise = str

main :: IO ()
main = do
  file <- xdgDirsFile
  case file of
    Just f -> putStrLn f
    Nothing -> putStrLn "ligm"
  -- callTofi ["amo", "gus"] >>= putStrLn

-- main :: IO ()
-- main = do
--     maybeValue <- lookupEnv "MY_ENV_VAR"  -- unwrap the IO (Maybe String)
--     case maybeValue of
--         Just value -> putStrLn $ "The environment variable is: " ++ value
--         Nothing    -> putStrLn "The environment variable is not set."
