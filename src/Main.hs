module Main where

import XMonad
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import qualified Data.Map as M

main :: IO ()
main = xmonad def { modMask  = mod4Mask
                  , terminal = "urxvt"  
                  , focusedBorderColor  = "#7cafc2"
                  , normalBorderColor   = "#181818"
                  , focusFollowsMouse   = True
                  , keys     = keybindings <+> keys def
                  }

keybindings conf@(XConfig { XMonad.modMask = modm }) = M.fromList
  [ ((modm, xK_F12), runOrRaisePrompt $ def { position = Top } )
  ]
