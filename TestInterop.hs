module Main (main) where

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
  (a_stdin, a_stdout, a_handle) <- startProcessWithPipes a_exec a_args
  (b_stdin, b_stdout, b_handle) <- startProcessWithPipes b_exec b_args

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

  -- Clean up.
  _ <- hClose a_stdin
  _ <- hClose b_stdin
  _ <- hClose a_stdout
  _ <- hClose b_stdout
  a_result <- waitForProcess a_handle
  b_result <- waitForProcess b_handle

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
