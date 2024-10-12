module CliArgs (parseArgs) where

import Options

-- TODO: make the error messages good
parseArg :: Options -> String -> Options
parseArg opts "-c"
  | File _ <- save opts = error "Cannot use both clipboard and file."
  | otherwise = opts { save = Clipboard }
parseArg opts "-f"
  | save opts == Clipboard = error "Cannot use both clipboard and file."
  | otherwise = opts { save = File Nothing }
parseArg opts ('-':'f':'=':path)
  | save opts == Clipboard = error "Cannot use both clipboard and file."
  | otherwise = opts { save = File (Just path)}
parseArg _ a = error $ "Invalid argument " ++ a

parseArgs :: [String] -> Options
parseArgs =
  foldl parseArg defaultOptions
