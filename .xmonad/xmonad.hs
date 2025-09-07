--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import Data.Map qualified as M
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import HsLua.REPL (setup)
import System.Exit
import System.Process
import XMonad
import XMonad.Actions.CycleWS qualified as C
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.NoBorders
import XMonad.Actions.Search as S
import XMonad.Actions.SpawnOn
import XMonad.Actions.Submap
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks)
import XMonad.Hooks.WindowSwallowing
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.StackSet qualified as W
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

myXPConfig :: XPConfig
myXPConfig =
  def
    { font = "xft:monospace:size=12",
      bgColor = "black",
      fgColor = "#00FF00",
      borderColor = "#00FFFF",
      bgHLight = "#FFFF00",
      fgHLight = "black",
      alwaysHighlight = True,
      promptBorderWidth = 2,
      position = Top,
      height = 40,
      historySize = 30,
      historyFilter = deleteConsecutive,
      defaultText = ""
    }

-- Search Prompts (custom)
engToTel = S.searchEngine "eng-to-tel" "https://translate.google.co.in/?sl=auto&tl=te&text="

pip = S.searchEngine "pip" "https://pypi.org/search/?q="

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth = 3

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
altMask :: KeyMask
altMask = mod1Mask

superMask :: KeyMask
superMask = mod4Mask

myModMask = superMask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor = "#888"

myFocusedBorderColor = "#0ff"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.

myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    [ -- workspace 1
      ((modm .|. controlMask, xK_1), spawn "alacritty"),
      -- workspace 2
      ((modm .|. controlMask, xK_2), spawn "emacs" >> spawn "calibre"),
      -- workspace 3
      ((modm .|. controlMask, xK_3), spawn "brave"),
      -- workspace 4
      ((modm .|. controlMask, xK_4), spawn "kitty"),
      -- workspace 5
      ((modm .|. controlMask, xK_5), spawn "qbittorrent"),
      -- workspace 9
      ((modm .|. controlMask, xK_9), spawn "kitty"),
      -- launch a terminal
      ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),
      -- launch rofi
      ((modm, xK_p), spawn "rofi -show drun"),
      ((modm, xK_o), spawn "rofi -modi emoji -show emoji"),
      -- close focused window
      ((modm .|. shiftMask, xK_c), kill),
      -- Rotate through the available layout algorithms
      ((modm, xK_space), sendMessage NextLayout),
      --  Reset the layouts on the current workspace to default
      ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
      -- Resize viewed windows to the correct size
      ((modm, xK_r), refresh),
      -- Move focus to the next window
      -- , ((modm,               xK_Tab   ), windows W.focusDown)

      -- Move focus to the next window
      ((modm, xK_j), windows W.focusDown),
      ((modm .|. shiftMask, xK_q), windows $ W.focusDown),
      -- Move focus to the previous window
      ((modm, xK_k), windows W.focusUp),
      -- map to workspace 9 & 8
      ((modm, xK_q), windows $ W.greedyView "9"),
      ((modm, xK_a), windows $ W.greedyView "8"),
      -- Move focus to the master window
      ((modm, xK_m), windows W.focusMaster),
      -- Swap the focused window and the master window
      ((modm, xK_Return), windows W.swapMaster),
      -- Swap the focused window with the next window
      ((modm .|. shiftMask, xK_j), windows W.swapDown),
      -- Swap the focused window with the previous window
      ((modm .|. shiftMask, xK_k), windows W.swapUp),
      -- Shrink the master area
      ((modm, xK_h), sendMessage Shrink),
      -- Expand the master area
      ((modm, xK_l), sendMessage Expand),
      -- Push window back into tiling
      ((modm, xK_t), withFocused $ windows . W.sink),
      -- Increment the number of windows in the master area
      ((modm, xK_comma), sendMessage (IncMasterN 1)),
      -- Deincrement the number of windows in the master area
      ((modm, xK_period), sendMessage (IncMasterN (-1))),
      -- Toggle the status bar gap
      -- Use this binding with avoidStruts from Hooks.ManageDocks.
      -- See also the statusBar function from Hooks.DynamicLog.
      ((modm .|. shiftMask, xK_f), sendMessage ToggleStruts),
      -- Quit xmonad
      -- , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

      -- Restart xmonad
      ((modm, xK_z), spawn "xmonad --recompile; xmonad --restart"),
      -- , ((modm  , xK_q     ), spawn "xmonad --recompile; killall xmobar; xmonad --restart")

      -- Run xmessage with a summary of the default keybindings (useful for beginners)
      ((modm .|. shiftMask, xK_slash), spawn ("echo \"" ++ help ++ "\" | xmessage -file -")),
      {- ==== custom keybindings ==== -}
      -- , ((modm .|. controlMask, xK_q), removeWorkspace)
      {- volume -}
      ((0, xF86XK_AudioMute), spawn "~/.scripts/volume.sh mute"),
      ((0, xF86XK_AudioLowerVolume), spawn "~/.scripts/volume.sh down"),
      ((0, xF86XK_AudioRaiseVolume), spawn "~/.scripts/volume.sh up"),
      ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl set +10%"),
      ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 10%-"),
      -- go to previous workspace
      ((modm, xK_Tab), C.toggleWS),
      ((modm, xK_Down), C.prevWS),
      ((modm, xK_Up), C.nextWS),
      ((modm .|. shiftMask, xK_Down), C.shiftToPrev),
      ((modm .|. shiftMask, xK_Up), C.shiftToNext),
      {- scratchpads -}
      ((modm .|. controlMask, xK_Return), namedScratchpadAction myScratchPads "terminal"),
      ((modm .|. controlMask, xK_m), namedScratchpadAction myScratchPads "mpv"),
      ((modm .|. controlMask, xK_h), namedScratchpadAction myScratchPads "htop"),
      ((modm .|. controlMask, xK_a), namedScratchpadAction myScratchPads "pavucontrol"),
      ((modm .|. controlMask, xK_s), namedScratchpadAction myScratchPads "spotify"),
      ((modm .|. controlMask, xK_c), namedScratchpadAction myScratchPads "copyq"),
      ( (modm, xK_s),
        submap . M.fromList $
          [ ((0, xK_y), promptSearch myXPConfig S.youtube),
            ((0, xK_g), promptSearch myXPConfig S.google),
            ((0, xK_w), promptSearch myXPConfig S.wikipedia),
            ((0, xK_i), promptSearch myXPConfig S.images),
            ((0, xK_m), promptSearch myXPConfig S.maps),
            ((0, xK_d), promptSearch myXPConfig S.duckduckgo),
            ((0, xK_e), promptSearch myXPConfig S.dictionary),
            ((0, xK_t), promptSearch myXPConfig engToTel),
            ((0, xK_p), promptSearch myXPConfig pip)
          ]
      ),
      ( (modm .|. shiftMask, xK_s),
        submap . M.fromList $
          [ ((0, xK_s), spawn "~/.scripts/screenshot.sh select"),
            ((0, xK_p), spawn "~/.scripts/screenshot.sh select-good"),
            ((0, xK_w), spawn "~/.scripts/screenshot.sh window"),
            ((0, xK_c), spawn "~/.scripts/screenshot.sh clipboard"),
            ((0, xK_t), spawn "~/.scripts/extract-text-from-image.sh")
          ]
      ),
      ( (modm .|. shiftMask, xK_v),
        submap . M.fromList $
        [ ((0, xK_y), spawn "~/.scripts/download-yt-video-and-mpv.sh") ]
      )
    ]
      ++
      --
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --
      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1),
        ( \w ->
            focus w
              >> mouseMoveWindow w
              >> windows W.shiftMaster
        )
      ),
      -- mod-button2, Raise the window to the top of the stack
      ((modm, button2), (\w -> focus w >> windows W.shiftMaster)),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (modm, button3),
        ( \w ->
            focus w
              >> mouseResizeWindow w
              >> windows W.shiftMaster
        )
      )
      -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
-- myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
myLayout = tiled ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2

    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100

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
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat,
      resource =? "desktop_window" --> doIgnore,
      resource =? "kdesktop" --> doIgnore
    ]
    <+> namedScratchpadManageHook myScratchPads

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook

--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = swallowEventHook (className =? "kitty") (return True)

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.

-- By default, do nothing.
myStartupHook = do
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom --experimental-backends &"

myScratchPads :: [NamedScratchpad] -- use 'xprop' for window className
myScratchPads =
  [ NS "terminal" spawnTerm findTerm manageTerm,
    NS "mpv" (myTerminal ++ " -t mpv") (title =? "mpv") (customFloating $ W.RationalRect (1 / 4) (1 / 4) (2 / 4) (2 / 4)),
    NS "htop" (myTerminal ++ " -t htop -e htop") (title =? "htop") manageTerm,
    NS "pavucontrol" "pavucontrol" (className =? "pavucontrol") (customFloating $ W.RationalRect (1 / 4) (1 / 4) (2 / 4) (2 / 4)),
    NS "copyq" "copyq" (className =? "copyq") manageTerm,
    NS "spotify" "spotify" (className =? "Spotify") manageTerm
  ]
  where
    spawnTerm = myTerminal ++ " -t scratchpad"
    findTerm = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  xmproc <- spawnPipe "xmobar -x 0 ~/.dotfiles/.config/xmobar/xmobarrc"
  xmonad $ docks defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults =
  def
    { -- simple stuff
      terminal = myTerminal,
      focusFollowsMouse = myFocusFollowsMouse,
      clickJustFocuses = myClickJustFocuses,
      borderWidth = myBorderWidth,
      modMask = myModMask,
      workspaces = myWorkspaces,
      normalBorderColor = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,
      -- key bindings
      keys = myKeys,
      mouseBindings = myMouseBindings,
      -- hooks, layouts
      layoutHook = avoidStruts $ smartBorders $ myLayout,
      manageHook = myManageHook,
      handleEventHook = myEventHook,
      logHook = myLogHook,
      startupHook = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help =
  unlines
    [ "The default modifier key is 'alt'. Default keybindings:",
      "",
      "-- launching and killing programs",
      "mod-Shift-Enter  Launch xterminal",
      "mod-p            Launch dmenu",
      "mod-Shift-p      Launch gmrun",
      "mod-Shift-c      Close/kill the focused window",
      "mod-Space        Rotate through the available layout algorithms",
      "mod-Shift-Space  Reset the layouts on the current workSpace to default",
      "mod-n            Resize/refresh viewed windows to the correct size",
      "",
      "-- move focus up or down the window stack",
      "mod-Tab        Move focus to the next window",
      "mod-Shift-Tab  Move focus to the previous window",
      "mod-j          Move focus to the next window",
      "mod-k          Move focus to the previous window",
      "mod-m          Move focus to the master window",
      "",
      "-- modifying the window order",
      "mod-Return   Swap the focused window and the master window",
      "mod-Shift-j  Swap the focused window with the next window",
      "mod-Shift-k  Swap the focused window with the previous window",
      "",
      "-- resizing the master/slave ratio",
      "mod-h  Shrink the master area",
      "mod-l  Expand the master area",
      "",
      "-- floating layer support",
      "mod-t  Push window back into tiling; unfloat and re-tile it",
      "",
      "-- increase or decrease number of windows in the master area",
      "mod-comma  (mod-,)   Increment the number of windows in the master area",
      "mod-period (mod-.)   Deincrement the number of windows in the master area",
      "",
      "-- quit, or restart",
      "mod-Shift-q  Quit xmonad",
      "mod-q        Restart xmonad",
      "mod-[1..9]   Switch to workSpace N",
      "",
      "-- Workspaces & screens",
      "mod-Shift-[1..9]   Move client to workspace N",
      "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
      "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
      "",
      "-- Mouse bindings: default actions bound to mouse events",
      "mod-button1  Set the window to floating mode and move by dragging",
      "mod-button2  Raise the window to the top of the stack",
      "mod-button3  Set the window to floating mode and resize by dragging"
    ]
