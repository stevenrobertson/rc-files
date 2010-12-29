#!/usr/bin/runhaskell

import System.IO
import Control.Concurrent
import Data.List
import Text.Printf

microseconds = (* 1000000) . truncate
minSec :: Int -> (Int, Int)
minSec t = (t `div` 60, t `mod` 60)

main = estimator 10 0.99 1000 =<< getDirty

estimator :: Double -> Double -> Double -> Double -> IO ()
estimator period alpha est val = do
    newVal <- getDirty
    let delta = (val - newVal) / period
        newEst = est * alpha + delta * (1 - alpha)
        trem = truncate $ newVal / newEst
    putStrLn $ uncurry (printf "New estimate: %d:%02d") $ minSec trem
    threadDelay $ microseconds period
    estimator period alpha newEst newVal

getDirty :: IO Double
getDirty = do
    contents <- readFile "/proc/meminfo"
    let Just dirty = lookup "Dirty" . map (span (/= ':')) $ lines contents
    return . read . takeWhile (/= ' ') . dropWhile (== ' ') $ tail dirty
