{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards,
             NoMonomorphismRestriction, TemplateHaskell, FlexibleContexts #-}

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run(spawnPipe, runProcessWithInput)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutHints
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.Combo
import XMonad.Layout.WindowNavigation (Navigate(..), windowNavigation)
import XMonad.Config.Gnome
import XMonad.Actions.WindowBringer
import System.IO
import System.Process
import System.Environment
import System.FilePath

import Data.Monoid
import Data.Ratio
import Data.List
import Data.Word
import Control.Monad
import Network.BSD
import Language.Haskell.TH

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
        where (r1, r2) = splitHorizontallyBy (2/3) rect
              (r1a, r1b) = splitVerticallyBy frac r1

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

data Host = Isis | IsisSecondary | Anubis | Aten | Ptah
            deriving (Read, Show, Eq)

vertTabbed  = windowNavigation (combineTwo (Mirror $ Tall 1 0.05 0.75)
                                simpleTabbed simpleTabbedBottom)
horizTabbed = windowNavigation (combineTwo (TwoPane 0.03 0.33)
                                simpleTabbed simpleTabbed)
ptahLayouts = vertTabbed ||| Mirror (Tall 1 0.05 0.75)
isisLayouts = horizTabbed ||| CodingLayout (3/5) 1
isisSecLayouts = GridRatio 1 ||| Tall 1 0.01 0.7
defaultLayouts = Tall 1 0.01 0.5 ||| GridRatio 1.2

-- This must be derived at compile-time, since the actual type of the layout
-- changes based on which layouts are used in it (and the types are
-- incompatible). Worse, since a TH splice can't call another, both the host and
-- the layout must be determined in the same splice. This is uggo.
hostAndLayouts = $( do
    name <- runIO getHostName
    disp <- runIO $ getEnv "DISPLAY"
    case name of
         "isis"     -> if '1' `elem` disp || '2' `elem` disp
                          then [| (IsisSecondary, isisSecLayouts) |]
                          else [| (Isis, isisLayouts) |]
         "ptah"     -> [| (Ptah, ptahLayouts) |]
         "anubis"   -> [| (Anubis, defaultLayouts) |]
         "aten"     -> [| (Aten, defaultLayouts) |]
    )
host = fst hostAndLayouts
layouts = snd hostAndLayouts ||| simpleTabbed

modm = mod4Mask

normWorkspaces  = map show [1..4]
shiftWorkspaces = map show [5..8]
myWorkspaces nScreens = withScreens nScreens $ normWorkspaces ++ shiftWorkspaces

browser = case host of
               IsisSecondary    -> "firefox -P secondary"
               _                -> "firefox"

myManageHook = composeAll
                [ className =? "qemu-system-x86_64" --> doFloat
                , className =? "Do"                 --> doFloat
                , className =? "Display"            --> doFloat
                , title     =? "MusicBrainz lookup" --> doFloat
                , title     =? "cuburn"             --> doFloat
                , title     =? "ChangeScreen"       --> doFullFloat
                , isFullscreen                      --> doFullFloat ]

myKeys =
    [ ((modm, xK_i), spawn browser)
    , ((modm, xK_c), spawn "google-chrome")
    , ((modm, xK_t), spawn "xterm -e screen -m")
    , ((modm .|. shiftMask, xK_t), withFocused $ windows . W.sink)
    , ((modm, xK_Left),     sendMessage $ Move L)
    , ((modm, xK_Right),    sendMessage $ Move R)
    , ((modm, xK_Up),       sendMessage $ Move U)
    , ((modm, xK_Down),     sendMessage $ Move D)
    , ((modm, xK_BackSpace), namedScratchpadAction scratchpads "screen")
    , ((modm, xK_n),        namedScratchpadAction scratchpads "notes")
    ] ++
    [ ((m, k), windows $ onCurrentScreen f i) |
        (i, k) <- zip normWorkspaces [xK_F1 .. xK_F6],
        (f, m) <- [(W.greedyView, 0), (W.shift, modm)]] ++
    [ ((m, k), windows $ onCurrentScreen f i) |
        (i, k) <- zip shiftWorkspaces [xK_F1 .. xK_F6],
        (f, m) <- [(W.greedyView, shiftMask), (W.shift, shiftMask .|. modm)]]




xmobarCmd cfg scr = unwords [".cabal/bin/xmobar",
                             "-x", show (fromIntegral scr),
                             ".xmobarrc-" ++ cfg]
launchBar n =
    if host == Isis && n == 0
       then spawnPipe ".cabal/bin/xmobar"
       else spawnPipe $ xmobarCmd cfg n
  where cfg = case host of
                   Ptah            -> "ptah" ++ show (fromIntegral n)
                   Isis            -> "isis-sec"
                   IsisSecondary   -> "isis-sec"
                   Anubis          -> "anubis"
                   Aten            -> "anubis"

myXmobarPP screenNo outhnd = xmobarPP {
    ppOutput    = hPutStrLn outhnd,
    ppTitle     = xmobarColor "#551155" "" . shorten 140,
    ppUrgent    = xmobarColor "" "#ff0000" . snd . unmarshall,
    ppWsSep     = "",
    ppCurrent   = (++" ") . xmobarColor "#000000" "#aaaaaa" . snd . unmarshall,
    ppHidden    = ppHidden xmobarPP . filtScr,
    ppVisible   = filtScr,
    ppSort      = getSortByTag,
    ppSep       = "  ",
    ppLayout    = xmobarColor "#223355" "" . head . reverse . words
    }
  where
    filtScr "NSP" = ""
    filtScr wname = case i of
      screenNo  -> n ++ " "
      _         -> xmobarColor "#cc55cc" "#cccccc" (n ++ " ")
      where (i, n) = unmarshall wname

scratchpads =
    [ NS "screen" "xterm -T scratchpad -xrm 'XTerm*allowTitleOps: False' -e screen -S scratch -d -R"
         (title =? "scratchpad") float
    , NS "notes" "gvim -c 'cd ~/notes' --role notes"
         (role =? "notes") float
    ]
  where
    role = stringProperty "WM_WINDOW_ROLE"
    float = customFloating (W.RationalRect 0.25 0.25 0.5 0.5)

main = do
    nScreens <- countScreens
    bars <- mapM launchBar [0 .. nScreens - 1]
    gnomeRegister
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        workspaces  = myWorkspaces nScreens,
        startupHook = setWMName "LG3D",
        manageHook  = myManageHook
                   <+> manageDocks
                   <+> namedScratchpadManageHook scratchpads
                   <+> manageHook defaultConfig,
        layoutHook  = avoidStruts . smartBorders $ layouts,
        logHook = mapM_ (dynamicLogWithPP . uncurry myXmobarPP) $ zip [0..] bars,
        handleEventHook = mappend handleFocusEvent $
                          handleEventHook defaultConfig,
        borderWidth = 1,
        normalBorderColor = "#e1e1e1",
        focusedBorderColor = "#4466aa",
        modMask     = modm
        } `additionalKeys` myKeys


