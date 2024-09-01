module Main where

import Distribution.Client.DistDirLayout
import Distribution.Client.HttpUtils
import Distribution.Client.ProjectConfig
import Distribution.Client.RebuildMonad
import Distribution.Verbosity
import System.Directory
import System.Environment

main :: IO ()
main = do
    [root] <- getArgs
    let dd = defaultDistDirLayout (ProjectRootImplicit root) Nothing Nothing
        projectConfigFp = distProjectFile dd ""
    exists <- doesFileExist projectConfigFp
    putStrLn ("Does project file exist? " ++ show exists ++ " Path: " ++ projectConfigFp)
    httpTransport <- configureTransport normal [] Nothing
    runRebuild root (readProjectFileSkeleton minBound httpTransport dd "" "project")
    putStrLn ("Parser passed: " ++ root)
