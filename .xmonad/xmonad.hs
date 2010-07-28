{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe, runProcessWithInput)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutHints
import System.IO
import System.Process
import System.Environment

import Data.Monoid
import Data.Ratio
import Control.Monad

-- Port of ThreeCol layout to a sensationally arbitrary number of columns
data NCol a = NCol { nCol           :: !Int
                   , nColNMaster    :: !Int
                   , nColDelta      :: !Rational
                   , nColFrac       :: !Rational }
    deriving (Show,Read)

instance LayoutClass NCol a where
    pureLayout (NCol cols nmaster _ frac) rect stack = zip ws rs
        where ws = W.integrate stack
              rs = tileN cols frac rect nmaster (length ws)
    handleMessage l m =
        return $ msum [fmap resize      (fromMessage m)
                      ,fmap incmastern  (fromMessage m)]
            where resize Shrink = l { nColFrac = max d $ f-d }
                  resize Expand = l { nColFrac = min 1 $ f+d }
                  incmastern (IncMasterN x) = l { nColNMaster = max 1 (n+x) }
                  n = nColNMaster l
                  d = nColDelta l
                  f = nColFrac l
    description (NCol c _ _ _) = (show c) ++ "Col"

tileN :: Int -> Rational -> Rectangle -> Int -> Int -> [Rectangle]
tileN cols frac rect nmaster n
    | n <= nmaster = splitVertically n rect
    | otherwise = splitVertically nmaster r1 ++
                  tileNSlaves cols' (n - nmaster) r2
    where cols' = min (n - nmaster) (cols - 1)
          (r1, r2) = splitHorizontallyBy frac rect

-- Layout just how I like it on my 30"
data CodingLayout a = CodingLayout { masterFrac :: !Rational }
    deriving (Show, Read)

instance LayoutClass CodingLayout a where
    pureLayout (CodingLayout frac) rect stack = zip ws rs
        where ws = W.integrate stack
              rs = tileCoding frac rect $ length ws
    description _ = "CodingLayout"

tileCoding :: Rational -> Rectangle -> Int -> [Rectangle]
tileCoding frac rect n
    | n == 1    = [rect]
    | n == 2    = [r1, r2]
    | n < 6     = r1 : tileNSlaves 2 (n-1) r2
    | otherwise = r1a : splitHorizontally 2 r1b ++ tileNSlaves 2 (n-3) r2
        where (r1, r2) = splitHorizontallyBy frac rect
              (r1a, r1b) = splitVerticallyBy (3/5) r1

-- Tile N windows across a number of columns. If the number of windows don't
-- divide evenly across the number of columns, push the (n `mod` cols) extra
-- windows into the last rows, to declutter the left side of the window
-- (on the assumption that the most active part of the screen is the master
-- column and those columns closest to it).
tileNSlaves :: Int -> Int -> Rectangle -> [Rectangle]
tileNSlaves cols n rect =
    concat . map (uncurry splitVertically) . zip numPerCol $ splitHorizontally cols rect
    where numPerCol = replicate (cols - n `mod` cols) (n `div` cols) ++
                      replicate (n `mod` cols) (n `div` cols + 1)

-- Aggressively grab focus.
aggFocus :: Window -> X ()
aggFocus w = local (\c -> c { mouseFocused = True }) $ withWindowSet $ \s -> do
    let stag = W.tag . W.workspace
        curr = stag $ W.current s
    mnew <- maybe (return Nothing) (fmap (fmap stag) . uncurry pointScreen)
            =<< asks mousePosition
    root <- asks theRoot
    case () of
        _ | W.member w s -> windows (W.focusWindow w)
          | Just new <- mnew, w == root -> windows (W.view new)
          | otherwise -> return ()

handleFocusEvent :: Event -> X All
handleFocusEvent e@(CrossingEvent {ev_window = w, ev_event_type = t})
    | t == enterNotify && ev_mode e == notifyNormal && ev_same_screen e
    = do
        aggFocus w
        return $ All False

handleFocusEvent _ = return $ All True



-- The actual configuration begins below --


-- Config file is shared across desktop and laptop; host-specific configuration
-- is all done here

data Host = Isis | IsisSecondary | Anubis

getHostname :: IO Host
getHostname = do
    -- XMonad might be intercepting the process with an extra wait call?
    -- out <- readProcess "hostname" [] ""
    host <- fmap (head . words) $ runProcessWithInput "hostname" [] ""
    disp <- getEnv "DISPLAY"
    return $ case host of
        "anubis"                    -> Anubis
        "isis" | '1' `elem` disp    -> IsisSecondary
        "isis"                      -> Isis

{- I can't figure out what type this needs to be to allow host-based switching
myLayouts :: l a
myLayouts Isis = GridRatio (5/4) ||| CodingLayout (186/360) |||
                 ThreeCol 1 (4/360) (188/360) ||| Full
myLayouts IsisSecondary = GridRatio (5/4) ||| Full
myLayouts Anubis = ThreeColMid 1 (3/100) (1/4) ||| GridRatio (5/4) ||| Full
-}

myLayouts _ = GridRatio (5/4) ||| layoutHints (CodingLayout (186/360)) |||
              ThreeCol 1 (4/360) (188/360) ||| Full

modm = mod4Mask

normWorkspaces = map show [1..6]
shiftWorkspaces = map show [7..12]
myWorkspaces = normWorkspaces ++ shiftWorkspaces

browser IsisSecondary = "chromium-bin"
browser host = "firefox"

myManageHook = composeAll
                [ className =? "qemu-system-x86_64" --> doFloat
                , className =? "Do"                 --> doFloat ]



myKeys host =
    [
        ((modm, xK_i), spawn (browser host)),
        ((modm, xK_t), spawn "xterm"),
        ((modm .|. shiftMask, xK_t), withFocused $ windows . W.sink)
    ] ++
    [ ((m, k), windows $ f i) |
        (i, k) <- zip normWorkspaces [xK_F1 .. xK_F6],
        (f, m) <- [(W.greedyView, 0), (W.shift, modm)]] ++
    [ ((m, k), windows $ f i) |
        (i, k) <- zip shiftWorkspaces [xK_F1 .. xK_F6],
        (f, m) <- [(W.greedyView, shiftMask), (W.shift, shiftMask .|. modm)]]

launchBar Isis = spawnPipe "/home/steven/.cabal/bin/xmobar"
launchBar IsisSecondary =
    spawnPipe "/home/steven/.cabal/bin/xmobar /home/steven/.xmobarrc-isis-sec"
launchBar Anubis =
    spawnPipe "/home/steven/.cabal/bin/xmobar /home/steven/.xmobarrc-anubis"

myXmobarPP = xmobarPP {
    ppTitle  = xmobarColor "#ddddff" "" . shorten 80,
    ppSep    = "  ",
    ppLayout = \lay ->
        if lay == "Full"
            then xmobarColor "#000000" "#cc1111" " Full "
            else xmobarColor "#667799" "#000000" lay
    }

main = do
    host <- getHostname
    xmobar <- launchBar host
    xmonad $ defaultConfig {
        workspaces  = myWorkspaces,
        manageHook  = myManageHook <+> manageDocks <+> manageHook defaultConfig,
        layoutHook  = avoidStruts  $  smartBorders $ myLayouts host,
        logHook = dynamicLogWithPP myXmobarPP {ppOutput = hPutStrLn xmobar},
        handleEventHook = mappend handleFocusEvent $
                          handleEventHook defaultConfig,
        borderWidth = 1,
        modMask     = modm
        } `additionalKeys` (myKeys host)


