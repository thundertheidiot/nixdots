import System.Process ( readProcess )
import System.IO ( writeFile )
import Data.Function ((&))

wg :: String -> String -> IO String
wg arg stdin =
  init <$> readProcess "wg" [arg] stdin

genkey = wg "genkey" ""
genpsk = wg "genpsk" ""
pubkey = wg "pubkey"

encrypt :: String -> IO String
encrypt = readProcess "sops" ["encrypt", "--filename-override", "a.bin"]

makeKeys :: String -> IO ()
makeKeys name = do
  key <- genkey
  pubkey <- pubkey key
  pskey <- genpsk
  
  writeFile (name ++ "-wg-pubkey") pubkey
  encrypt key >>= writeFile (name ++ "-wg-privkey")
  encrypt pskey >>= writeFile (name ++ "-wg-pskey")
  return ()

main = 
  mapM_ makeKeys ["home", "vps"]
