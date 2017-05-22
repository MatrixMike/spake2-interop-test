module Main (main) where

import qualified Data.List as List (break, head, take, drop)

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

main :: IO ()
main = do
  args <- Environment.getArgs

  case List.break ("--" ==) args of
    (a_exec:a_args, sep:b_exec:b_args) -> do
      -- Launch the two processes.
      let a = (proc a_exec a_args){
            std_in = CreatePipe,
            std_out = CreatePipe
            }
      let b = (proc b_exec b_args){
            std_in = CreatePipe,
            std_out = CreatePipe
            }
      aProcess <- createProcess a
      bProcess <- createProcess b

      case (aProcess, bProcess) of
        ((Just a_stdin, Just a_stdout, _, a_handle),
         (Just b_stdin, Just b_stdout, _, b_handle)) -> do

          -- Read the first SPAKE2 message from each.
          _ <- putStrLn "Gonna get line a"
          a_start <- hGetLine a_stdout
          _ <- putStrLn $ "Got " ++ a_start ++ ". Gonna get line b"
          b_start <- hGetLine b_stdout
          _ <- putStrLn $ "Got " ++ b_start ++ ". Got lines"

          -- Send them along to each other.
          putStrLn "Gonna put line a"
          hPutStrLn a_stdin b_start
          hFlush a_stdin
          hPutStrLn b_stdin b_start
          hFlush b_stdin
          putStrLn "Put the lines"

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

        otherwise -> do
          putStrLn "Some error."
          Exit.exitWith (Exit.ExitFailure 1)

    otherwise -> do
      putStrLn "Usage: interop <argv for a> -- <argv for b>"
      Exit.exitWith (Exit.ExitFailure 1)
