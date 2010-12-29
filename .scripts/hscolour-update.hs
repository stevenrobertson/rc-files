#!/usr/bin/runhaskell

import Control.Applicative
import Data.List
import Data.Maybe
import System.Posix
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO.Error

haddockdir home = home </> ".cabal/share/doc"
tmpdir = "/tmp/hscolour-update"
pkgname = reverse . tail . dropWhile (/= '-') . reverse

main = do
    hdir <- fmap haddockdir getHomeDirectory
    pkgs <- nubBy (\a b -> pkgname a == pkgname b)
              . filter ((\h -> '.' /= h && '{' /= h) . head)
              . reverse . sort
              . catMaybes . map (stripPrefix "    ") . lines
             <$> readProcess "ghc-pkg" ["list"] []
    let doesNeedDocs pkg = not `fmap` doesDirectoryExist (hdir </> pkg </> "html/src")
    needsDocs <- map fst . filter snd . zip pkgs <$> mapM doesNeedDocs pkgs
    if null needsDocs then exitWith ExitSuccess else do
        createDirectoryIfMissing True tmpdir
        mapM_ (makeDocs hdir) needsDocs
        removeDirectoryRecursive tmpdir

makeDocs hdir pkgspec = do
    putStrLn $ "Processing " ++ pkgspec
    try $ do
        sout <- readProcess "cabal" ["unpack", "-d", tmpdir, pkgspec] []
        case stripPrefix "Unpacking to " $ takeWhile (/= '\n') sout of
            Just dir -> buildDocs dir
            Nothing  -> putStrLn $ "Unpacking " ++ pkgspec ++ " failed."
  where
    cabal dir args =
        createProcess ((proc "cabal" args) { cwd = Just $ tmpdir </> dir }) >>=
            \(_,_,_,procHnd) -> waitForProcess procHnd
    buildDocs dir = do
        cabal dir ["configure"]
        cabal dir ["haddock", "--hyperlink-source", "--hoogle"]
        let srcdir = tmpdir </> dir </> "dist/doc/html" </> pkgname pkgspec
            dstdir = hdir </> "share/doc" </> pkgspec </> "html"
        rawSystem "mkdir" ["-p", dstdir]
        rawSystem "cp" ["-r", srcdir, dstdir]
        removeDirectoryRecursive $ tmpdir </> dir

