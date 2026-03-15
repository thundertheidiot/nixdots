{-# LANGUAGE LambdaCase #-}

module Nix.Build where

import System.Exit
import System.IO
import System.Posix.IO (createPipe, fdToHandle)
import System.Process (
    CreateProcess (..),
    StdStream (..),
    createProcess,
    proc,
    waitForProcess,
 )

lastLineWithAction' :: Handle -> String -> (String -> IO ()) -> IO String
lastLineWithAction' handle line action =
    hIsEOF handle >>= \case
        True -> return line
        False -> do
            line <- hGetLine handle
            action line
            lastLineWithAction' handle line action

printLines :: Handle -> IO String
printLines handle = lastLineWithAction' handle "" putStrLn

nixBuild :: [String] -> Bool -> IO (Maybe String)
nixBuild args inEmacs = do
    (read', write') <- createPipe
    write <- fdToHandle write'
    read <- fdToHandle read'

    (_, _, _, nix) <-
        createProcess
            ( proc "nix" $
                ["build", "--print-out-paths"]
                    ++ ( if inEmacs
                            then []
                            else ["--log-format", "internal-json", "-v"]
                       )
                    ++ args
            )
                { std_out = UseHandle write
                , std_err = UseHandle write
                }

    hSetBuffering read LineBuffering

    if inEmacs
        then do
            output <- printLines read
            exitCode <- waitForProcess nix
            case exitCode of
                ExitSuccess -> return $ Just output
                ExitFailure _ -> return Nothing
        else do
            (Just nomStdin, Just nomStdout, _, nom) <-
                createProcess
                    (proc "nom" ["--json"])
                        { std_in = CreatePipe
                        , std_out = CreatePipe
                        }

            output <- lastLineWithAction' read "" $ hPutStrLn nomStdin

            putStrLn =<< hGetContents nomStdout

            exitCode <- waitForProcess nix
            _ <- waitForProcess nom

            case exitCode of
                ExitSuccess -> return $ Just output
                ExitFailure _ -> return Nothing

main = do
    out <- nixBuild [".#nixosConfigurations.ooo.config.system.build.toplevel"] False
    putStrLn $ "ligm " ++ show out ++ "   a"
