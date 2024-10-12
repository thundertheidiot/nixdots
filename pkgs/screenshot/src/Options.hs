module Options where

data SaveMode = SaveModeNull | Clipboard | File (Maybe String) 
  deriving (Eq, Show)

data Options = Options
  { save :: SaveMode
  } deriving Show

optionsSanityCheck :: Options -> Options
optionsSanityCheck opts
  | save opts == SaveModeNull =
    error "Error: You must set a save mode and a selection mode."
  | otherwise = opts

defaultOptions :: Options
defaultOptions = Options
  { save = SaveModeNull 
  }
