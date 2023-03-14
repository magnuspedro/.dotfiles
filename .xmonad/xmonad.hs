-- Data
import Data.List (isInfixOf, isPrefixOf)
import Data.Monoid (Endo)
import Data.Time.Calendar
import Data.Time.Clock
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import XMonad
-- Actions
import XMonad.Actions.CycleWS (Direction1D (..), WSType (..), moveTo, nextScreen, prevScreen, shiftTo)
import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Actions.WithAll (killAll, sinkAll)
-- Hooks
import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, shorten, wrap, xmobarColor, xmobarPP, xmobarStrip)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks, docksEventHook, manageDocks)
import XMonad.Hooks.SetWMName
-- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.LimitWindows (decreaseLimit, increaseLimit, limitWindows)
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, single, (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import qualified XMonad.Layout.ToggleLayouts as T (ToggleLayout (Toggle), toggleLayouts)
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), windowArrange)
import XMonad.Layout.WindowNavigation
-- Utils
import XMonad.Prompt
import XMonad.Prompt.DirExec (dirExecPrompt)
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

modm :: KeyMask
modm = mod4Mask

workspace :: [WorkspaceId]
workspace = ["dev", "www", "tools", "mus", "..."]

defaultFont :: String
defaultFont = "xft:Mononoki:size=10:antialias=true:hinting=true,xft:FontAwesome:size=10"

defaultTerminal :: String
defaultTerminal = "alacritty"

startup :: X ()
startup = do
  setWMName "LG3D"
  spawnOnce "/usr/bin/dunst &"
  spawnOnce "/usr/bin/picom&"
  spawnOnce "feh --bg-fill ~/Pictures/Wallpaper/gruvbox_blackhole_super_scale.png &"
  spawnOnce "xmodmap ~/.xmodmap &"

tall =
  renamed [Replace "tall"] $
    spacing 3 $
      limitWindows 5 $
        ResizableTall 1 (3 / 100) (1 / 2) []

monocle =
  renamed [Replace "full"] $
    noBorders $
      limitWindows 5
        Full

floats =
  renamed [Replace "flts"] $
    noBorders
      simplestFloat

tabs =
  renamed [Replace "tabs"] $
    tabbed shrinkText tabTheme

threeRow =
  renamed [Replace "threeRow"] $
    smartBorders $
      windowNavigation $
        addTabs shrinkText tabTheme $
          subLayout [] (smartBorders Simplest) $
            limitWindows 7
            -- Mirror takes a layout and rotates it by 90 degrees.
            -- So we are applying Mirror to the ThreeCol layout.
            $
              Mirror $
                ThreeCol 1 (3 / 100) (1 / 2)

wideAccordion =
  renamed [Replace "wideAccordion"] $
    Mirror Accordion

-- setting colors for tabs layout and tabs sublayout.
tabTheme =
  def
    { fontName = defaultFont,
      activeColor = "#83a598",
      inactiveColor = "#282828",
      activeBorderColor = "#458588",
      inactiveBorderColor = "#584945",
      activeTextColor = "#584945",
      inactiveTextColor = "#fbf1c7"
    }

-- The layout hook
layout =
  avoidStruts $
    mouseResize $
      windowArrange $
        T.toggleLayouts floats $
          mkToggle (NBFULL ?? NOBORDERS ?? EOT) defaultLayout
  where
    defaultLayout =
      tall
        ||| monocle
        ||| floats
        ||| noBorders tabs
        ||| wideAccordion
        ||| threeRow

keyBinds :: [(String, X ())]
keyBinds =
  [ -- XMonad
    ("M-C-r", spawn "xmonad --recompile"), -- Recompiles xmonad
    ("M-S-r", spawn "xmonad --restart"), -- Restarts xmonad
    ("M-S-q", io exitSuccess), -- Quits xmonad
    ("M-S-c", kill), -- kill client
    ("M-b", sendMessage ToggleStruts), -- Toggle xmobar
    ("M-f", sendMessage (T.Toggle "floats")), -- Toggles my 'floats' layout
    ("M-t", withFocused $ windows . W.sink), -- Tile client again
    ("M-S-t", sinkAll), -- Push ALL floating windows to tile
    ("M-m", windows W.swapMaster), -- Set master
    ("M-n", sendMessage MirrorExpand), -- expand tile
    ("M-S-n", sendMessage MirrorShrink), -- shrink tile
    ("M-S-p", spawn "arandr"),
    ("M-j", windows W.focusUp),
    ("M-k", windows W.focusDown),
    ("M-S-j", windows W.swapUp),
    ("M-S-k", windows W.swapDown),
    -- Rofi
    ("M-p", spawn "rofi -modi drun -show drun -theme gruvbox-dark-hard"),
    -- Workspaces
    ("M-.", nextScreen), -- Switch focus to next monitor
    ("M-,", prevScreen), -- Switch focus to prev monitor

    -- Override
    ("M-<Return>", spawn defaultTerminal),
    -- Media Keys
    ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 5%-"),
    ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 5%+"),
    ("<XF86AudioMute>", spawn "amixer -q sset Master toggle"),
    ("<XF86MonBrightnessDown>", spawn "~/.dotfiles/brightness.sh - eDP-1 0.1"),
    ("<XF86MonBrightnessUp>", spawn "~/.dotfiles/brightness.sh + eDP-1 0.1"),
    ("<XF86AudioPrev>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"),
    ("<XF86AudioNext>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"),
    ("<XF86AudioPlay>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"),
    ("<XF86AudioStop>", spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Stop"),
    -- PrintScreen
    ("<Print>", spawn "flameshot gui -p /home/magnus/Pictures/Screenshots")
  ]

removeKeybinds :: [String]
removeKeybinds =
  [ "M-S-<Return>",
    "M-S-q",
    "M-q"
  ]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

appConfig :: XMonad.Query (Data.Monoid.Endo WindowSet)
appConfig =
  insertPosition Below Newer
    <+> composeAll
      [ -- Firefox
        title =? "Mozilla Firefox" --> doShift (workspace !! 1),
        (className =? "firefox" <&&> resource =? "Dialog") --> doFloat,
        (className =? "Places") --> doFloat,
        -- Spotify
        className =? "Spotify" --> doShift (workspace !! 3),
        -- Gimp
        stringProperty "WM_WINDOW_ROLE" =? "gimp-message-dialog" --> doFloat,
        -- Generic
        title =? "float" --> doFloat
      ]

main = do
  xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"
  xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.config/xmobar/xmobarrc"
  xmonad $
    ewmh $
      docks $
        def
          { modMask = modm,
            focusFollowsMouse = False,
            borderWidth = 3,
            terminal = defaultTerminal,
            focusedBorderColor = "#b16286",
            normalBorderColor = "#ebdbb2",
            layoutHook = layout,
            startupHook = startup,
            workspaces = workspace,
            manageHook = appConfig,
            logHook =
              dynamicLogWithPP
                xmobarPP
                  { ppOutput = \x ->
                      hPutStrLn xmproc0 x
                        >> hPutStrLn xmproc1 x,
                    ppCurrent = xmobarColor "#b8bb26" "", -- Current workspace in xmobar
                    ppVisible = xmobarColor "#d3869b" "", -- Visible but not current ws
                    ppHidden = xmobarColor "#d3869b" "", -- Hidden workspaces in xmobar
                    ppHiddenNoWindows = xmobarColor "#928374" "", -- Hidden workspaces (no windows)
                    ppTitle = xmobarColor "#bdae93" "" . shorten 60, -- Title of active window
                    ppSep = " | ", -- Separators in xmobar
                    ppUrgent = xmobarColor "#fb4934" "#50945" . wrap "!" "!", -- Urgent workspace
                    ppExtras = [windowCount], -- # of windows current workspace
                    ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
                  }
          }
          `removeKeysP` removeKeybinds
          `additionalKeysP` keyBinds
