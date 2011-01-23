#!/usr/bin/runhaskell
{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

import Control.Applicative
import Data.Foldable (foldrM)
import Data.List
import System.Posix
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO.Error

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

data Settings = Settings
    { haddockDirs   :: [FilePath]
    , dstDir        :: FilePath
    , tmpDir        :: FilePath
    } deriving (Eq, Ord, Show)

-- A package name, including version number.
type Pkg = B.ByteString
-- A map of package names to their dependencies.
type DepMap = M.Map Pkg [Pkg]

pkgname = reverse . tail . dropWhile (/= '-') . reverse

-- Turn the output of 'ghc-pkg dot' into a DepMap. Makes strong assumptions about
-- format of ghc-pkg output.
readPkgDot  :: DepMap -> [Pkg] -> DepMap
readPkgDot m [] = m
readPkgDot m (x:xs)
    | "\"" `B.isPrefixOf` x =
        let (_:src:_:dst:_) = B.split '"' x
        in  M.alter (Just . (dst :) . maybe [] id) src .
            M.alter (Just . maybe [] id) dst $ readPkgDot m xs
    | otherwise = readPkgDot m xs

-- Run a DFS on the DepMap, popping visited nodes after handling.
processPkgMap :: Settings -> DepMap -> IO DepMap
processPkgMap stgs m
    | M.null m  = return M.empty
    | otherwise =
        let ((pkg, deps), m') = M.deleteFindMin m
        in processPkgMap stgs =<< processPkg stgs pkg =<< foldrM go m' (deps)
  where
    go pkg m' = case M.lookup pkg m' of
        Nothing     -> return m'
        Just deps   -> processPkg stgs pkg =<< foldrM go (M.delete pkg m') deps

-- Make documentation for a package.
processPkg :: Settings -> Pkg -> DepMap -> IO DepMap
processPkg stgs@(Settings {tmpDir, dstDir}) pkgB m = do
    needed <- fmap not $ doesDirectoryExist dstdir'
    if needed then go else return (Right ())
    return m
  where
    pkg = B.unpack pkgB
    dstdir' = dstDir </> pkg </> "html"
    go = do
        putStrLn $ "Processing " ++ pkg
        try $ do
            sout <- readProcess "cabal" ["unpack", "-d", tmpDir, pkg] []
            case stripPrefix "Unpacking to " $ takeWhile (/= '\n') sout of
                Just dir -> buildDocs dir
                Nothing  -> putStrLn $ "Unpacking " ++ pkg ++ " failed."
    cabal dir args =
        createProcess ((proc "cabal" args) { cwd = Just $ tmpDir </> dir }) >>=
            \(_,_,_,procHnd) -> waitForProcess procHnd
    buildDocs dir = do
        cabal dir ["configure"]
        cabal dir ["haddock", "--hyperlink-source", "--hoogle"]
        let srcdir = tmpDir </> dir </> "dist/doc/html" </> pkgname pkg
        rawSystem "mkdir" ["-p", dstdir']
        rawSystem "cp" ["-r", srcdir, dstdir']
        createProcess ((proc "cabal" ["install"]) { cwd = Just $ "/home/steven/haskell/dummy"}) >>= \(_,_,_,procHnd) -> waitForProcess procHnd
        removeDirectoryRecursive $ tmpDir </> dir


main = do
    userDocDir <- (</> "share/doc") <$> getAppUserDataDirectory "cabal"
    stgs <- Settings [userDocDir] userDocDir . (</> "hscolour-update")
         <$> getTemporaryDirectory
    {-
    cabalDev <- doesDirectoryExist "cabal-dev"
    let vstr = showVersion compilerVersion
        extraArgs = case cabalDev of
            True -> ["-f", "cabal-dev/packages-" ++ vstr ++ ".conf"]
            _    -> []
    -}
    let extraArgs = []

    pkgs <- readPkgDot M.empty . B.lines . B.pack
         <$> readProcess "ghc-pkg" ("dot":"--user":extraArgs) []
    processPkgMap stgs pkgs
    putStrLn "All done?"

{-
    let doesNeedDocs pkg =
            not `fmap` doesDirectoryExist (dstDir stgs </> pkg </> "html/src")
    needsDocs <- map fst . filter snd . zip pkgs <$> mapM doesNeedDocs pkgs
    if null needsDocs then exitWith ExitSuccess else do
        createDirectoryIfMissing True tmpdir
        mapM_ () needsDocs
        removeDirectoryRecursive $ tmpDir stgs
-}
