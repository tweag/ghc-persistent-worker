module Main where

import GhcServer.GenProject (writeProject)
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir, depthStr]
      | [(depth, "")] <- reads depthStr, depth > 0 -> do
          createDirectoryIfMissing True dir
          writeProject dir depth
          let levels = 2 * depth
              modules = sum [2 ^ (l + 1) | l <- [0 .. levels - 1]] :: Int
              units = 2 * depth
          putStrLn ("Generated project in " ++ dir)
          putStrLn ("  depth:   " ++ show depth ++ " (" ++ show levels ++ " levels)")
          putStrLn ("  units:   " ++ show units)
          putStrLn ("  modules: " ++ show modules)
    _ -> do
      putStrLn "Usage: gen-project <directory> <depth>"
      putStrLn ""
      putStrLn "  <directory>  Target directory for the generated project"
      putStrLn "  <depth>      Tree depth multiplier (total levels = 2 * depth)"
      putStrLn ""
      putStrLn "Example: gen-project /tmp/test-project 2"
      putStrLn "  Creates a project with 4 levels, 4 units, and 30 modules"
