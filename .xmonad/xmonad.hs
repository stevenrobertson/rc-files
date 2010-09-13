{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, NoMonomorphismRestriction #-}

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Util.Run(spawnPipe, runProcessWithInput)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutHints
import XMonad.Config.Gnome
import XMonad.Actions.WindowBringer
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
    | n < 5     = r1 : tileNSlaves 2 (n-1) r2
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

data Host = Isis | IsisSecondary | Anubis | Aten deriving (Read, Show, Eq)

getHostname :: IO Host
getHostname = do
    -- XMonad might be intercepting the process with an extra wait call?
    -- out <- readProcess "hostname" [] ""
    host <- fmap (head . words) $ runProcessWithInput "hostname" [] ""
    disp <- getEnv "DISPLAY"
    return $ case host of
        "anubis"                    -> Anubis
        "aten"                      -> Aten
        "isis" | '1' `elem` disp    -> IsisSecondary
        "isis"                      -> Isis

-- Different Layout types can't be used with 'if'.
data ExtChoice l1 l2 a = ExtChoice Bool (l1 a) (l2 a) deriving (Show, Read)

instance (LayoutClass l1 a, LayoutClass l2 a) => LayoutClass (ExtChoice l1 l2) a where
    runLayout (W.Workspace i (ExtChoice True  l1 l2) ms) r = do
        fmap (\(wrs, mlt') -> (wrs, fmap (flip (ExtChoice True) l2) mlt')) $
            runLayout (W.Workspace i l1 ms) r
    runLayout (W.Workspace i (ExtChoice False l1 l2) ms) r = do
        fmap (\(wrs, mlt') -> (wrs, fmap (ExtChoice False l1) mlt')) $
            runLayout (W.Workspace i l2 ms) r

    description (ExtChoice True  l1 l2) = description l1
    description (ExtChoice False l1 l2) = description l2

    handleMessage (ExtChoice True  l1 l2) m = do
        fmap (fmap (flip (ExtChoice True) l2)) $ handleMessage l1 m
    handleMessage (ExtChoice False l1 l2) m = do
        fmap (fmap $ ExtChoice False l1) $ handleMessage l2 m

myLayouts host =
    ExtChoice (host == Isis) isisLayouts $
    ExtChoice (host == IsisSecondary) isisSecLayouts anubisLayouts
    where
        isisLayouts =   Tall 1 (1/100) (50/100) |||
                        layoutHints (CodingLayout (186/360)) |||
                        ThreeCol 1 (4/360) (186/360) |||
                        NCol 4 1 (1/100) (25/100) ||| Full
        isisSecLayouts = GridRatio (5/4) ||| Full
        anubisLayouts = Tall 1 (1/100) (50/100) ||| GridRatio (5/4) ||| Full

modm = mod4Mask

normWorkspaces = map show [1..6]
shiftWorkspaces = map show [7..12]
myWorkspaces = normWorkspaces ++ shiftWorkspaces

browser IsisSecondary = "chromium-bin"
browser host = "firefox"

myManageHook = composeAll
                [ className =? "qemu-system-x86_64" --> doFloat
                , className =? "Do"                 --> doFloat
                , isFullscreen                      --> doFullFloat ]

myKeys host =
    [
        ((modm, xK_i), spawn (browser host)),
        ((modm, xK_t), spawn "gnome-terminal"),
        ((modm .|. shiftMask, xK_t), withFocused $ windows . W.sink),
        ((modm, xK_g), gotoMenu' "dmenul")
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
launchBar Aten = launchBar Anubis

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
    gnomeRegister
    xmonad $ defaultConfig {
        workspaces  = myWorkspaces,
        startupHook = setWMName "LG3D",
        manageHook  = myManageHook <+> manageDocks <+> manageHook defaultConfig,
        layoutHook  = avoidStruts  $  smartBorders $ myLayouts host,
        logHook = dynamicLogWithPP myXmobarPP {ppOutput = hPutStrLn xmobar},
        handleEventHook = mappend handleFocusEvent $
                          handleEventHook defaultConfig,
        borderWidth = 1,
        modMask     = modm
        } `additionalKeys` (myKeys host)


