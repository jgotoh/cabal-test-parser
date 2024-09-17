module Main where

import Control.Exception (SomeException, try)
import Control.Monad
import Distribution.Client.DistDirLayout
import Distribution.Client.HttpUtils
import Distribution.Client.ProjectConfig
import Distribution.Client.RebuildMonad
import Distribution.Verbosity
import System.Directory
import System.Environment (getArgs)
import System.FilePath ((</>))

main :: IO ()
main = do
    root <- getProjectsRoot
    projects <- listProjectPaths root
    print $ "Checking directory " <> root <> " containing " <> show (length projects) <> " projects"
    checkProjects projects

getProjectsRoot :: IO FilePath
getProjectsRoot = do
    args <- getArgs
    let root = case args of
            (x : _) -> x
            [] -> error "Missing argument path to repositories directory"
    exists <- doesDirectoryExist root
    unless exists $ error $ "Root directory does not exist: " <> root
    return root

listProjectPaths :: FilePath -> IO [FilePath]
listProjectPaths path = do
    contents <- listDirectory path
    filterM doesDirectoryExist $ (path </>) <$> contents

checkProjects :: [FilePath] -> IO ()
checkProjects fps = do
    httpTransport <- configureTransport normal [] Nothing
    forM_ fps $ \root -> do
        checkProject httpTransport root

checkProject :: HttpTransport -> FilePath -> IO ()
checkProject httpTransport root = do
    let dd = defaultDistDirLayout (ProjectRootImplicit root) Nothing Nothing
        projectConfigFp = distProjectFile dd ""
    exists <- doesFileExist projectConfigFp
    putStrLn ("Does project file exist? " ++ show exists ++ " Path: " ++ projectConfigFp)
    result <- try (void (runRebuild root (readProjectFileSkeleton normal httpTransport dd "" "project"))) :: IO (Either SomeException ())
    case result of
        Right _ -> putStrLn ("Parser passed: " ++ root)
        Left ex -> putStrLn ("Parser failed: " ++ show ex)
