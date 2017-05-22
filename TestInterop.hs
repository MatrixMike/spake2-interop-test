module Main (main) where

import Control.Exception (bracket)
import qualified Data.List as List (break)

import System.IO (hGetLine, hPutStr, hPutStrLn, hFlush, hClose)

import System.Process (
    StdStream(CreatePipe)
  , CreateProcess(CreateProcess, std_in, std_out)
  , proc
  , createProcess
  , waitForProcess
  )

import qualified System.Environment as Environment (getArgs)
import qualified System.Exit as Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)

unexpectedError :: String -> IO a
unexpectedError msg = do
  putStrLn msg
  Exit.exitWith (Exit.ExitFailure 1)

startProcessWithPipes exec args = do
  let desc = proc exec args
  let desc' = desc{ std_in = CreatePipe
                  , std_out = CreatePipe
                  }
  process <- createProcess desc
  case process of
    (Just stdin, Just stdout, _, handle) -> pure (stdin, stdout, handle)
    err -> unexpectedError "Bug in createProcess, didn't return handles"

withProcess exec args action =
  bracket
    (startProcessWithPipes exec args)
    closeFDs
    action
  where
    closeFDs (inH, outH, processH) = do
      hClose inH
      hClose outH
      waitForProcess processH

parseArgs :: [String] -> Maybe ((String, [String]), (String, [String]))
parseArgs args =
  let (before, after) = List.break ("--" ==) args in
  case (before, after) of
    (a_exec:a_args, sep:b_exec:b_args) -> Just ((a_exec, a_args), (b_exec, b_args))
    _ -> Nothing

main :: IO ()
main = do
  args <- Environment.getArgs
  ((a_exec, a_args), (b_exec, b_args)) <- case parseArgs args of
                                            Nothing -> unexpectedError "Usage: interop <argv for a> -- <argv for b>"
                                            Just x -> pure x

  -- Launch the two processes.
  (a_key, b_key) <- withProcess
    a_exec a_args
    (\(a_stdin, a_stdout, a_handle) ->
       withProcess
         b_exec b_args
         (\(b_stdin, b_stdout, b_handle) -> do
             -- Read the first SPAKE2 message from each.
             a_start <- hGetLine a_stdout
             b_start <- hGetLine b_stdout

             -- Send them along to each other.
             _ <- hPutStrLn a_stdin b_start
             _ <- hFlush a_stdin
             _ <- hPutStrLn b_stdin a_start
             _ <- hFlush b_stdin

             -- Read the SPAKE2 session key computed by each.
             a_key <- hGetLine a_stdout
             b_key <- hGetLine b_stdout
             return (a_key, b_key)
             ))

  -- Report the computed SPAKE2 session keys and whether or not they match.
  putStrLn $ "A's key: " ++ a_key
  putStrLn $ "B's key: " ++ b_key
  case a_key == b_key of
    True -> do
      putStrLn "Session keys match."
      Exit.exitWith Exit.ExitSuccess

    False -> do
      putStrLn "Session keys mis-match."
      Exit.exitWith (Exit.ExitFailure 1)
