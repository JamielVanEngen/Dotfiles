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

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 4

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- NOTE: from 0.9.1 on numlock mask is set automatically. The numlockMask
-- setting should be removed from configs.
--
-- You can safely remove this even on earlier xmonad versions unless you
-- need to set it to something other than the default mod2Mask, (e.g. OSX).
--
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
-- myNumlockMask   = mod2Mask -- deprecated in xmonad-0.9.1
------------------------------------------------------------


-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myNormalBorderColor  = "#3c3836"
myFocusedBorderColor = "#d5c4a1"
myppCurrent = "#427b58"
myppHidden = "#268bd2"
myppTitle = "#b57614"
myppUrgent = "#9d0006"
myWorkspaces = ["1","2","3","4","5","6","7","8","NSP"]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

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
	("M-S-s", namedScratchpadAction myScratchPads "spotify"),

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

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
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

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- * NOTE: XMonad.Hooks.EwmhDesktops users must remove the obsolete
-- ewmhDesktopsLayout modifier from layoutHook. It no longer exists.
-- Instead use the 'ewmh' function from that module to modify your
-- defaultConfig as a whole. (See also logHook, handleEventHook, and
-- startupHook ewmh notes.)
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

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
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm,
    NS "spotify" spawnSpot findSpot manageSpot
    ]
    where
        spawnTerm  = myTerminal ++ " --class scratchpad"
	findTerm   = resource =? "scratchpad"
	manageTerm = customFloating $ W.RationalRect l t w h
	    where
	        h = 0.9
	        w = 0.9
	        t = 0.95 -h
	        l = 0.95 -w
	spawnSpot  = "spotify"
	findSpot   = resource =? "spotify"
	manageSpot = customFloating $ W.RationalRect l t w h
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
------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.
--

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add initialization of EWMH support to your custom startup
-- hook by combining it with ewmhDesktopsStartup.
--

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
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do 
  xmproc <- spawnPipe "xmobar -x 0 /home/jamiel/.config/xmobar/xmobarrc"
  xmonad $ docks def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        -- numlockMask deprecated in 0.9.1
        -- numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
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


-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
