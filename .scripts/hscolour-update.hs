#!/usr/bin/runhaskell

import Control.Applicative
import Data.List
import System.Posix
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO.Error

haddockdir home = home </> ".cabal/share/doc"
tmpdir = "/tmp/hscolour-update"
pkgname = reverse . dropWhile (/= '-') . reverse

main = do
    hdir <- fmap haddockdir getHomeDirectory
    contents <- nubBy (\a b -> pkgname a == pkgname b)
              . filter (('.' /=) . head)
              . reverse . sort
             <$> getDirectoryContents hdir
    let needsDocs fp = getFileStatus (hdir </> fp) >>= \fs ->
            if isDirectory fs
               then not `fmap` doesDirectoryExist (hdir </> fp </> "html/src")
               else return False
    needsTest <-  map fst . filter snd . zip contents
              <$> mapM needsDocs contents
    if null needsTest then exitWith ExitSuccess else do
        createDirectoryIfMissing True tmpdir
        mapM_ makeDoc needsTest

makeDoc pkgname = do
    putStrLn $ "Processing " ++ pkgname
    try $ do
        sout <- readProcess "cabal" ["unpack", "-d", tmpdir, pkgname] []
        let Just dir = stripPrefix "Unpacking to " $ takeWhile (/= '\n') sout
            cabal args = createProcess ((proc "cabal" args)
                                        { cwd = Just $ tmpdir </> dir })
                       >>= \(_,_,_,procHnd) -> waitForProcess procHnd
        cabal ["configure"]
        cabal ["build"]
        cabal ["haddock", "--hyperlink-source"]
        cabal ["copy"]
        removeDirectoryRecursive $ tmpdir </> dir


