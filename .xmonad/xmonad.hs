{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, NoMonomorphismRestriction #-}

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run(spawnPipe, runProcessWithInput)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Scratchpad
import qualified XMonad.StackSet as W
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutHints
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IndependentScreens
import XMonad.Config.Gnome
import XMonad.Actions.WindowBringer
import System.IO
import System.Process
import System.Environment
import System.FilePath

import Data.Monoid
import Data.Ratio
import Data.List
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
data CodingLayout a = CodingLayout { codingMFrac   :: !Rational
                                   , codingNMaster :: !Int }
    deriving (Show, Read)

instance LayoutClass CodingLayout a where
    pureLayout (CodingLayout frac nm) rect stack = zip ws rs
        where ws = W.integrate stack
              rs = tileCoding frac nm rect $ length ws
    description _ = "CodingLayout"
    handleMessage l m =
        return $ msum [fmap incmastern  (fromMessage m)
                      ,fmap resize      (fromMessage m)]
          where incmastern (IncMasterN x) = l { codingNMaster = max 0 (n+x) }
                resize Shrink = l { codingMFrac = max 0 (f-1/50) }
                resize Expand = l { codingMFrac = min 1 (f+1/50) }
                n = codingNMaster l
                f = codingMFrac   l

tileCoding :: Rational -> Int -> Rectangle -> Int -> [Rectangle]
tileCoding frac nm rect n
    | nm == 0   = r1 : tileNSlaves 2 (n-1) r2
    | n <= nm+1 = r1a : splitHorizontally (n-1) r1b
    | otherwise = r1a : splitHorizontally nm r1b ++ tileNSlaves 2 (n-nm-1) r2
        where (r1, r2) = splitHorizontallyBy horizFrac rect
              (r1a, r1b) = splitVerticallyBy frac r1
              -- make right side of screen support 2 80-col GVim windows
              horizFrac = (186/360)

-- Tile N windows across a number of columns. If the number of windows don't
-- divide evenly across the number of columns, push the (n `mod` cols) extra
-- windows into the last rows, to declutter the left side of the window
-- (on the assumption that the most active part of the screen is the master
-- column and those columns closest to it).
tileNSlaves :: Int -> Int -> Rectangle -> [Rectangle]
tileNSlaves cols n rect
    | n == 0    = []
    | n < cols  = splitHorizontally n rect
    | otherwise = concat . map (uncurry splitVertically) . zip numPerCol $
                    splitHorizontally cols rect
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
        "isis" | '2' `elem` disp    -> IsisSecondary
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

myLayouts host = layoutHints $
    ExtChoice (host == Isis) isisLayouts $
    ExtChoice (host == IsisSecondary) isisSecLayouts anubisLayouts
    where
        isisLayouts =   CodingLayout (3/5) 1 |||
                        Tall 1 (1/100) (50/100) |||
                        ThreeCol 1 (4/360) (186/360) |||
                        NCol 4 1 (1/100) (25/100) ||| Full
        isisSecLayouts = GridRatio 1 ||| Tall 1 (1/100) (70/100) ||| Full
        anubisLayouts = Tall 1 (1/100) (50/100) ||| GridRatio (5/4) ||| Full

modm = mod4Mask

normWorkspaces  = map show [1..6]
shiftWorkspaces = map show [7..12]
myWorkspaces nScreens = withScreens nScreens $ normWorkspaces ++ shiftWorkspaces

browser IsisSecondary = "firefox -P secondary"
browser host = "firefox"

myManageHook = composeAll
                [ className =? "qemu-system-x86_64" --> doFloat
                , className =? "Do"                 --> doFloat
                , title     =? "MusicBrainz lookup" --> doFloat
                , title     =? "ChangeScreen"       --> doFullFloat
                , isFullscreen                      --> doFullFloat ]

myKeys host =
    [
        ((modm, xK_i), spawn (browser host)),
        ((modm, xK_t), spawn "gnome-terminal"),
        ((modm .|. shiftMask, xK_t), withFocused $ windows . W.sink),
        ((modm, xK_g), gotoMenu' "dmenul"),
        ((modm, xK_BackSpace), scratchpadSpawnActionCustom
            "xterm -name scratchpad -e screen -S scratch -d -R")
    ] ++
    [ ((m, k), windows $ onCurrentScreen f i) |
        (i, k) <- zip normWorkspaces [xK_F1 .. xK_F6],
        (f, m) <- [(W.greedyView, 0), (W.shift, modm)]] ++
    [ ((m, k), windows $ onCurrentScreen f i) |
        (i, k) <- zip shiftWorkspaces [xK_F1 .. xK_F6],
        (f, m) <- [(W.greedyView, shiftMask), (W.shift, shiftMask .|. modm)]]
    ++ [ ((modm, k), changeDisplay i) | (k, i) <- zip [xK_w, xK_e, xK_r] [2,0,1] ]
changeDisplay i = spawn $ "/home/steven/.scripts/change_display.py " ++ show i

xmobarCmd cfg scr = unwords ["/home/steven/.cabal/bin/xmobar",
                             "-x", show (fromIntegral scr),
                             "/home/steven/.xmobarrc-" ++ cfg]
launchBar Isis 0 = spawnPipe "/home/steven/.cabal/bin/xmobar"
launchBar host n = spawnPipe $ xmobarCmd cfg n
  where cfg = case host of
                   Isis            -> "isis-sec"
                   IsisSecondary   -> "isis-sec"
                   Anubis          -> "anubis"
                   Aten            -> "anubis"

myXmobarPP screenNo outhnd = xmobarPP {
    ppOutput    = hPutStrLn outhnd,
    ppTitle     = xmobarColor "#ddddff" "" . shorten 140,
    ppUrgent    = xmobarColor "" "#ff0000" . snd . unmarshall,
    ppWsSep     = "",
    ppCurrent   = \n -> (ppCurrent xmobarPP . snd $ unmarshall n) ++ " ",
    ppHidden    = ppHidden  xmobarPP . filtScr,
    ppVisible   = \_ -> "",
    ppSep       = "  ",
    ppLayout    = \lay ->
        if "Full" `isInfixOf` lay
            then xmobarColor "#000000" "#cc1111" " Full "
            else xmobarColor "#667799" "#000000" lay
    }
  where
    filtScr "NSP" = ""
    filtScr wname = (\(i,n) -> if i == screenNo then n++" " else "")
                    $ unmarshall wname

main = do
    host <- getHostname
    nScreens <- countScreens
    bars <- mapM (launchBar host) [0 .. nScreens - 1]
    gnomeRegister
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        workspaces  = myWorkspaces nScreens,
        startupHook = setWMName "LG3D",
        manageHook  = myManageHook
                   <+> manageDocks
                   <+> scratchpadManageHook (W.RationalRect 0.25 0.25 0.5 0.5)
                   <+> manageHook defaultConfig,
        layoutHook  = avoidStruts  $  smartBorders $ myLayouts host,
        logHook = mapM_ (dynamicLogWithPP . uncurry myXmobarPP) $ zip [0..] bars,
        handleEventHook = mappend handleFocusEvent $
                          handleEventHook defaultConfig,
        borderWidth = 1,
        modMask     = modm
        } `additionalKeys` (myKeys host)


