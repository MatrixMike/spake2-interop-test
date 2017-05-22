module Main (main) where

import qualified Data.List as List (break, head, take, drop)

import qualified System.IO as IO (hGetLine, hPutStrLn, hClose)

import qualified System.Process as Process (
    StdStream(CreatePipe)
  , CreateProcess(CreateProcess, std_in, std_out)
  , proc
  , createProcess
  , waitForProcess
  )

import qualified System.Environment as Environment (getArgs)
import qualified System.Exit as Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)

main :: IO ()
main = do
  args <- Environment.getArgs

  case List.break ("--" ==) args of
    (a_exec:a_args, sep:b_exec:b_args) -> do
      -- Launch the two processes.
      let a = (Process.proc a_exec a_args){
            Process.std_in = Process.CreatePipe,
            Process.std_out = Process.CreatePipe
            }
      let b = (Process.proc b_exec b_args){
            Process.std_in = Process.CreatePipe,
            Process.std_out = Process.CreatePipe
            }
      (a_stdin, a_stdout, _, a_handle) <- Process.createProcess a
      (b_stdin, b_stdout, _, b_handle) <- Process.createProcess b

      case (a_stdin, a_stdout, b_stdin, b_stdout) of
        (Just a_stdin, Just a_stdout, Just b_stdin, Just b_stdout) -> do
          -- Read the first SPAKE2 message from each.
          a_start <- IO.hGetLine a_stdout
          b_start <- IO.hGetLine b_stdout

          -- Send them along to each other.
          _ <- IO.hPutStrLn a_stdin b_start
          _ <- IO.hPutStrLn b_stdin a_start

          -- Read the SPAKE2 session key computed by each.
          a_key <- IO.hGetLine a_stdout
          b_key <- IO.hGetLine b_stdout

          -- Report the computed SPAKE2 session keys and whether or not they match.
          putStrLn $ "A's key: " ++ a_key
          putStrLn $ "B's key: " ++ b_key

          -- Clean up.
          _ <- IO.hClose a_stdin
          _ <- IO.hClose b_stdin
          _ <- IO.hClose a_stdout
          _ <- IO.hClose b_stdout
          a_result <- Process.waitForProcess a_handle
          b_result <- Process.waitForProcess b_handle

          -- Report.
          case a_key == b_key of
            True -> do
              putStrLn "Session keys match."
              Exit.exitWith Exit.ExitSuccess

            False -> do
              putStrLn "Session keys mis-match."
              Exit.exitWith (Exit.ExitFailure 1)

        otherwise -> do
          putStrLn "Some error."
          Exit.exitWith (Exit.ExitFailure 1)

    otherwise -> do
      putStrLn "Usage: interop <argv for a> -- <argv for b>"
      Exit.exitWith (Exit.ExitFailure 1)
