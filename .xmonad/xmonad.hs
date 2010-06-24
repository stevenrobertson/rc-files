import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import System.IO

modm = mod4Mask

myWorkspaces = map show [1..6]

myKeys =
    [
        ((modm, xK_i), spawn "chromium-bin"),
        ((modm, xK_t), spawn "xterm")
    ] ++
    [ ((m, k), windows $ f i) |
        (i, k) <- zip myWorkspaces [xK_F1 .. xK_F6],
        (f, m) <- [(W.greedyView, 0), (W.shift, modm)]]

myLayouts = ThreeColMid 1 (3/100) (1/2) ||| GridRatio (4/3) ||| GridRatio (5/4) ||| Full


main = do
    xmobar <- spawnPipe "xmobar"
    xmonad $ defaultConfig {
        workspaces  = myWorkspaces,
        manageHook  = manageDocks <+> manageHook defaultConfig,
        layoutHook  = avoidStruts  $  myLayouts,
        logHook = dynamicLogWithPP defaultPP { ppTitle  = shorten 90,
                                               ppLayout = (>> ""),
                                               ppOutput = hPutStrLn xmobar },
        borderWidth = 2,
        modMask     = modm
        } `additionalKeys` myKeys


