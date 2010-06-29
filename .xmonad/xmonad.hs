import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe, runProcessWithInput)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import System.IO
import System.Process

modm = mod4Mask

normWorkspaces = map show [1..6]
shiftWorkspaces = map show [7..12]
myWorkspaces = normWorkspaces ++ shiftWorkspaces

myKeys =
    [
        ((modm, xK_i), spawn "chromium"),
        ((modm, xK_t), spawn "xterm")
    ] ++
    [ ((m, k), windows $ f i) |
        (i, k) <- zip normWorkspaces [xK_F1 .. xK_F6],
        (f, m) <- [(W.greedyView, 0), (W.shift, modm)]] ++
    [ ((m, k), windows $ f i) |
        (i, k) <- zip shiftWorkspaces [xK_F1 .. xK_F6],
        (f, m) <- [(W.greedyView, shiftMask), (W.shift, shiftMask .|. modm)]]


myLayouts = ThreeColMid 1 (3/100) (1/2) ||| GridRatio (4/3) ||| GridRatio (5/4) ||| Full

launchBar = do
    -- XMonad might be intercepting the process with an extra wait call?
    -- out <- readProcess "hostname" [] ""
    out <- runProcessWithInput "hostname" [] ""
    let hostname = head $ words out
    if hostname == "anubis"
        then spawnPipe "/home/steven/.cabal/bin/xmobar /home/steven/.xmobarrc-anubis"
        else spawnPipe "xmobar"

main = do
    xmobar <- launchBar
    xmonad $ defaultConfig {
        workspaces  = myWorkspaces,
        manageHook  = manageDocks <+> manageHook defaultConfig,
        layoutHook  = avoidStruts  $  smartBorders $ myLayouts,
        logHook = dynamicLogWithPP xmobarPP { ppTitle  = shorten 90,
                                               ppLayout = (>> ""),
                                               ppOutput = hPutStrLn xmobar },
        borderWidth = 2,
        modMask     = modm
        } `additionalKeys` myKeys


