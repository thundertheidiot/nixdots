{-# LANGUAGE LambdaCase #-}
module Grim ( grimToClipboard
            , grimToFile
            , GrimError( Success
                       , GrimFailed
                       , WlCopyFailed
                       , DirectoryFailed ) ) where

import GHC.IO.Handle (Handle)

import System.Process ( readProcess
                      , ProcessHandle
                      , createProcess
                      , proc
                      , std_in
                      , std_out
                      , std_err
                      , StdStream ( CreatePipe
                                  , UseHandle)
                      , waitForProcess )

import System.Exit ( ExitCode ( ExitSuccess
                              , ExitFailure))

import System.IO ( openFile
                 , IOMode ( WriteMode
                          , ReadMode )
                 , hSetBinaryMode )
       

import qualified Data.ByteString as BS

data GrimError = Success | GrimFailed | WlCopyFailed | DirectoryFailed
  deriving Show

data Grim = GrimSuccess (Handle, ProcessHandle) | GrimFail 

getRegion :: IO String
getRegion = do
  reg <- readProcess "slurp" ["-o"] ""
  return $ init reg

grimStdout :: IO Grim
grimStdout = do
  reg <- getRegion
  stdin <- openFile "/dev/null" ReadMode
  stderr <- openFile "/dev/null" WriteMode
  
  createProcess (proc "grim" ["-g", reg, "-"])
    { std_out = CreatePipe
    , std_in = UseHandle stdin
    , std_err = UseHandle stderr
    } >>= \case
    (_, Just grimStdout, _, grimProc) -> do
      hSetBinaryMode grimStdout True
      return $ GrimSuccess (grimStdout, grimProc)
    _ -> return GrimFail

grimToClipboard :: IO GrimError
grimToClipboard =
  grimStdout >>= \case
  GrimSuccess (grimOut, grimProc) -> do
    stdout <- openFile "/dev/null" WriteMode
    stderr <- openFile "/dev/null" WriteMode
    createProcess (proc "wl-copy" ["-t", "image/png"])
      { std_out = UseHandle stdout
      , std_in = UseHandle grimOut
      , std_err = UseHandle stderr
      } >>= \case
      (_, _, _, wlProc) -> do
        grimExitCode <- waitForProcess grimProc
        wlExitCode <- waitForProcess wlProc

        return $ case (grimExitCode, wlExitCode) of
          (ExitSuccess, ExitSuccess) -> Success
          _ -> WlCopyFailed
    
  GrimFail -> return GrimFailed

grimToFile :: FilePath -> IO GrimError
grimToFile file = do
  grimStdout >>= \case
    GrimSuccess (grimOut, grimProc) -> do
      contents <- BS.hGetContents grimOut
      BS.writeFile file contents
      return Success
    GrimFail -> return GrimFailed
