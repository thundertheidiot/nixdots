{-# LANGUAGE LambdaCase #-}
module MenuArgs where

import Options
import Data.List (intercalate)
import System.Process (readProcess)

callTofi :: [String] -> IO String
callTofi args =
  readProcess "tofi" [] (intercalate "\n" args)

tofiToOpts :: IO Options
tofiToOpts = 
  callTofi ["clipboard"
           , "save" ] >>= \case
  "clipboard\n" -> return Options { save = Clipboard }
  "save\n" -> return Options { save = File Nothing }
  _ -> error "Error: nothing selected"
  
