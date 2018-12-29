module Main where

import qualified Data.Map as M
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers
import Graphics.X11.ExtraTypes.XF86
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Spacing
import XMonad.Layout.ShowWName
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Decoration
import XMonad.Hooks.EwmhDesktops

altMask = mod1Mask

main :: IO ()
main = xmonad $ ewmh myConfig

myConfig = def { modMask             = mod1Mask
               , terminal            = "urxvt"
               , focusedBorderColor  = "#7cafc2"
               , normalBorderColor   = "#181818"
               , focusFollowsMouse   = True
               --, manageHook          = composeOne [ className =? "pocket" -?> doCenterFloat ] -- placeHook (inBounds (underMouse (0, 0))) <+> manageHook def
               , layoutHook          = showWName $ spacingRaw True (Border 4 4 4 4) True (Border 4 4 4 4) True $ emptyBSP
               , keys                = keyBindings <+> keys def
               }

keyBindings conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList [ ((modm, xK_Up),    windows W.focusUp)              -- move between windows
             , ((modm, xK_Down),  windows W.focusDown)
             , ((modm, xK_Right), windows W.focusDown)
             , ((modm, xK_Left),  windows W.focusMaster)

             , ((modm, xK_q),     kill)

             , ((0, xF86XK_AudioMute),          spawn "amixer sset Master toggle")
             , ((0, xF86XK_AudioRaiseVolume),   spawn "amixer sset Master 5%+")
             , ((0, xF86XK_AudioLowerVolume),   spawn "amixer sset Master 5%-")

             , ((0, xF86XK_MonBrightnessUp),    spawn "xbacklight -inc 10")
             , ((0, xF86XK_MonBrightnessDown),  spawn "xbacklight -dec 10")

             , ((modm, xF86XK_Sleep),           spawn "systemctl hibernate")
             
             , ((0, xK_Scroll_Lock),            spawn "lockscreen")
             , ((0, xK_Print),                  spawn "gnome-screenshot")
             , ((modm, xK_space),               spawn "rofi -show drun -matching fuzzy -levenshtein-sort")
             , ((modm .|. shiftMask, xK_space), spawn "rofi -show window -matching fuzzy -levenshtein-sort")

             , ((modm .|. controlMask, xK_Right),   sendMessage $ ExpandTowards R)
             , ((modm .|. controlMask, xK_Left),    sendMessage $ ExpandTowards L)
             , ((modm .|. controlMask, xK_Down),    sendMessage $ ExpandTowards D)
             , ((modm .|. controlMask, xK_Up),      sendMessage $ ExpandTowards U)
             
             , ((modm .|. controlMask .|. shiftMask, xK_Right), sendMessage $ ShrinkFrom R)
             , ((modm .|. controlMask .|. shiftMask, xK_Left), sendMessage $ ShrinkFrom L)
             , ((modm .|. controlMask .|. shiftMask, xK_Down), sendMessage $ ShrinkFrom D)
             , ((modm .|. controlMask .|. shiftMask, xK_Up), sendMessage $ ShrinkFrom U)
             
             , ((modm,                 xK_r), sendMessage Rotate)
             , ((modm .|. controlMask, xK_r), sendMessage RotateL)
             , ((modm .|. altMask,     xK_r), sendMessage RotateR)
             
             , ((modm,                 xK_s), sendMessage Swap)

             , ((modm, xK_Tab), sendMessage FocusParent)
             , ((modm .|. shiftMask, xK_Tab), sendMessage SelectNode)
             , ((modm, xK_Escape), sendMessage MoveNode)
             ]
