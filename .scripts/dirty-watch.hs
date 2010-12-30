#!/usr/bin/runhaskell

import System.IO
import Control.Concurrent
import Data.List
import Text.Printf

microseconds = (* 1000000) . truncate

main = estimator 10 0.99 1000 =<< getDirty

estimator :: Double -> Double -> Double -> Double -> IO ()
estimator period alpha est val = do
    newVal <- getDirty
    let delta = (val - newVal) / period
        newEst = est * alpha + delta * (1 - alpha)
        trem = truncate $ newVal / newEst :: Int
    putStrLn $ printf "Time until cleared: %d:%02d (%1.1f kB/s)"
               (trem `div` 60) (trem `mod` 60) newEst
    threadDelay $ microseconds period
    estimator period alpha (max 1 newEst) newVal

getDirty :: IO Double
getDirty = do
    contents <- readFile "/proc/meminfo"
    let Just dirty = lookup "Dirty" . map (span (/= ':')) $ lines contents
    return . read . takeWhile (/= ' ') . dropWhile (== ' ') $ tail dirty
