import XMonad
import Data.Monoid
import System.Exit

import XMonad.Hooks.ManageDocks
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.Promote
import XMonad.Actions.MouseResize

import System.IO (hPutStrLn)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty

import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowArranger (windowArrange)
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.LimitWindows
import XMonad.Layout.Renamed
import XMonad.Util.NamedScratchpad

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

-- Variables
myTerminal      = "alacritty"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = False
myBorderWidth   = 4
myModMask       = mod4Mask
myNormalBorderColor  = "#3c3836"
myFocusedBorderColor = "#d5c4a1"
myppCurrent = "#427b58"
myppHidden = "#268bd2"
myppTitle = "#b57614"
myppUrgent = "#9d0006"
myWorkspaces = ["1","2","3","4","5","6","7","8","NSP"]

-- Key bindings. Add, modify or remove key bindings here.
myKeys :: [(String, X ())]
myKeys =
    [
	-- XMonad key 
	("M-q",  spawn "xmonad --recompile; xmonad --restart"),
	("M-S-q",  io exitSuccess),
	("M-<Return>", spawn (myTerminal)),
	("M-S-p", spawn "dmenu_run -h 24"), -- "rofi -theme dracula -show run"),
	-- Window killing
	("M-S-c", kill1),
	("M-S-a", killAll),

	-- Scratchpads
	("M-S-<Return>", namedScratchpadAction myScratchPads "terminal"),
	("M-S-b", namedScratchpadAction myScratchPads "firefox-dev"),
	("M-S-h", namedScratchpadAction myScratchPads "rest-client"),

	-- Window focus
	("M-f", sendMessage (T.Toggle "floats")),
	("M-<Delete>", withFocused $ windows . W.sink),
	("M-m", windows W.focusMaster),
	("M-j", windows W.focusDown),
	("M-k", windows W.focusUp),
	("M-S-j", windows W.swapDown),
	("M-S-k", windows W.swapUp),
	("M-S-m", promote),
	("M-<Tab>", sendMessage NextLayout),
	("M-S-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts),

        -- Resizing Windows
	("M-C-j", sendMessage MirrorShrink),
	("M-C-k", sendMessage MirrorExpand),

	-- Special keys
	("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute"),
	("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute"),
	("<XF86AudioMicMute>", spawn "amixer set Capture toggle"),
	("<XF86AudioMute>", spawn "amixer set Master toggle"),

	-- Program shortcuts
	("M-b", spawn "firefox &"),
	("M-p", spawn ("/home/jamiel/.config/dmenu/nerdtree-bookmarks.sh")),
	("M-r", spawn ("alacritty -e ranger &")),
	("M-o n", spawn "notion-app &"),
	("M-o k", spawn "keepassxc &"),
	("M-o d", spawn "discord &"),
	("M-o a", spawn "anki &")
    ]
	++
    [(otherModMasks ++ "M-" ++ key, action tag) | (tag, key)  <- zip myWorkspaces (map (\x -> "<F" ++ show x ++ ">") [1..12]), (otherModMasks, action) <- [ ("", windows . W.greedyView) -- or W.view
    , ("S-", windows . W.shift)]
    ]

-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-- Layouts:

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing' 8
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ limitWindows 20 Full


myLayout = avoidStruts $ mouseResize $ windowArrange $ smartBorders $
    mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
    where
    myDefaultLayout =      tall
                       ||| noBorders monocle
------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm dimensions,
    NS "spotify" spawnSpot findSpot dimensions,
    NS "firefox-dev" spawnFireDev findFireDev dimensions,
    NS "rest-client" spawnRestClient findRestClient dimensions
    ]
    where
        spawnTerm  = myTerminal ++ " --class scratchpad"
	findTerm   = resource =? "scratchpad"
	spawnSpot  = "spotify"
	findSpot   = resource =? "spotify"
	spawnFireDev  = "firefox-developer-edition"
	findFireDev   = className =? "firefoxdeveloperedition"
	spawnRestClient  = myTerminal ++ " --class restclient" 
	findRestClient   = resource =? "restclient" 
	dimensions = customFloating $ W.RationalRect l t w h
	    where
	        h = 0.9
	        w = 0.9
	        t = 0.95 -h
	        l = 0.95 -w

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "pavucontrol"    --> doFloat
    , resource  =? "*-settings"     --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore 
    ] <+> namedScratchpadManageHook myScratchPads
------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--
myEventHook :: Event -> X All
myEventHook = dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> floating)
    where floating  = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
-- Startup hook
myStartupHook = do
    spawnOnce "trayer --edge top --align right --widthtype request --padding 8 --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x333c3836 --height 23 --margin 250 &"
    spawnOnce "nitrogen --restore &"
    spawnOnce "ibus-daemon -drx &"
    spawnOnce "picom -b --config /home/jamiel/.config/picom/picom.conf &"
    spawnOnce "nm-applet &"
    spawnOnce "blueman-applet &"
    spawnOnce "mictray &"
    spawnOnce "redshift &"
    spawnOnce "xfce4-power-manager &"

main = do 
  xmproc <- spawnPipe "xmobar -x 0 /home/jamiel/.config/xmobar/xmobarrc"
  xmonad $ docks def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook,
        logHook            = dynamicLogWithPP xmobarPP  
                        { ppOutput = \x -> hPutStrLn xmproc x 
                        , ppCurrent = xmobarColor myppCurrent "" . wrap "[" "]" -- Current workspace in xmobar
                        , ppTitle = xmobarColor  myppTitle "" . shorten 80     -- Title of active window in xmobar
                        , ppUrgent = xmobarColor  myppUrgent "" . wrap "!" "!"  -- Urgent workspace
			, ppSep =  " | "
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
    } `additionalKeysP` myKeys

