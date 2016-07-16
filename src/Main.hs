module Main where

import qualified Data.Map as M
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.Place
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Hooks.DynamicLog


main :: IO ()
main = xmonad myConfig

myConfig = def { modMask             = mod4Mask
               , terminal            = "urxvt"
               , focusedBorderColor  = "#7cafc2"
               , normalBorderColor   = "#181818"
               , focusFollowsMouse   = True
               , manageHook          = placeHook (inBounds (underMouse (0, 0))) <+> manageHook def
               , keys                = keyBindings <+> keys def
               }

keyBindings conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList [ ((modm, xK_Up),    windows W.focusUp)              -- move between windows
             , ((modm, xK_Down),  windows W.focusDown)
             , ((modm, xK_Right), windows W.focusDown)
             , ((modm, xK_Left),  windows W.focusMaster)

             , ((modm .|. shiftMask, xK_Left), sendMessage Shrink) -- resize master/slave
             , ((modm .|. shiftMask, xK_Right), sendMessage Expand)
             ]
