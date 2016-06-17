module Main where

import System.Environment
import Http.Trace

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case null args of
    True  -> putStrLn $ "no url supplied!"
    False ->
      do 
        rdrs <- traceRedirects' (args!!0) True
        putStrLn $ "Redirects:"
        mapM_ (\x -> putStrLn $ "- " ++ x) rdrs
