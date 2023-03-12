-- Base

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

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["dev", "www", "tools", "mus", "..."]

myFont :: String
myFont = "xft:Mononoki:size=10:antialias=true:hinting=true,xft:FontAwesome:size=10"

myTerminal :: String
myTerminal = "alacritty" -- Set default terminal

myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  spawnOnce "/usr/bin/dunst &"
  spawnOnce "/usr/bin/picom&"
  spawnOnce "nitrogen --restore &"
  spawnOnce "xmodmap ~/.xmodmap &"

-- Windows manager

tall =
  renamed [Replace "tall"] $
    spacing 3 $ limitWindows 5 $
        ResizableTall 1 (3 / 100) (1 / 2) []

monocle =
  renamed [Replace "full"] $
    noBorders $
      limitWindows 5 $
        Full

floats =
  renamed [Replace "flts"] $
    noBorders $
      simplestFloat

tabs =
  renamed [Replace "tabs"] $
    tabbed shrinkText myTabTheme

threeRow =
  renamed [Replace "threeRow"] $
    smartBorders $
      windowNavigation $
        addTabs shrinkText myTabTheme $
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
myTabTheme =
  def
    { fontName = myFont,
      activeColor = "#83a598",
      inactiveColor = "#282828",
      activeBorderColor = "#458588",
      inactiveBorderColor = "#584945",
      activeTextColor = "#584945",
      inactiveTextColor = "#fbf1c7"
    }

-- The layout hook
myLayoutHook =
  avoidStruts $
    mouseResize $
      windowArrange $
        T.toggleLayouts floats $
          mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout =
      tall
        ||| monocle
        ||| floats
        ||| noBorders tabs
        ||| wideAccordion
        ||| threeRow

myXPConfig :: XPConfig
myXPConfig =
  def
    { font = "xft:Mononoki:size=10",
      bgColor = "#b8bb26",
      fgColor = "#1d2021",
      bgHLight = "#ebdbb2",
      fgHLight = "#fabd2f",
      borderColor = "#b8bb26",
      promptBorderWidth = 0,
      position = CenteredAt {xpCenterY = 0.2, xpWidth = 0.7},
      height = 50,
      historySize = 256,
      historyFilter = id,
      defaultText = [],
      autoComplete = Just 0, -- set Just 100000 for .1 sec
      showCompletionOnTab = True,
      searchPredicate = isPrefixOf,
      alwaysHighlight = True,
      maxComplRows = Nothing -- set to Just 5 for 5 rows
    }

-- -d: dimensions, -t: title
spawnFloatingTerm :: String -> X ()
spawnFloatingTerm cmd = spawn $ "alacritty " ++ opt ++ " -e" ++ cmd
  where
    opt = col ++ lin ++ posx ++ posy ++ floatDecorator
      where
        col = "-o window.dimensions.columns=200 " -- 123
        lin = "-o window.dimensions.lines=35 " -- 34
        posx = "-o window.position.x=60 " -- 10
        posy = "-o window.position.y=40 " -- 10
        floatDecorator = "-t \"float\""

replace :: Eq t => t -> t -> [t] -> [t]
replace a b = map (\c -> if c == a then b else c)

buildMaimString :: String -> String
buildMaimString = wrap pre post . replace ' ' '_' . replace ':' '-' . take 19
  where
    pre = "/usr/bin/maim -m 10 /home/maxim/bilder/screenshots/"
    post = ".png"

myKeys :: [(String, X ())]
myKeys =
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
    -- Rofi
    ("M-p", spawn "rofi -modi drun -show drun -theme gruvbox-dark-hard"),
    -- Workspaces
    ("M-.", nextScreen), -- Switch focus to next monitor
    ("M-,", prevScreen), -- Switch focus to prev monitor

    -- Override
    ("M-<Return>", spawn myTerminal),
    -- Media Keys
    ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 5%-"),
    ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 5%+"),
    ("<XF86AudioMute>", spawn "amixer -q sset Master toggle"),
    ("<XF86MonBrightnessDown>", spawn "~/.dotfiles/brightness.sh - eDP-1 0.1"),
    ("<XF86MonBrightnessUp>", spawn "~/.dotfiles/brightness.sh + eDP-1 0.1"),
    -- PrintScreen
    ("<Print>", spawn "flameshot gui -p /home/magnus/Pictures/Screenshots")
    -- Promt
    --("M-o", runOrRaisePrompt myXPConfig)
  ]

myRemKeys :: [String]
myRemKeys =
  [ "M-S-<Return>",
    "M-S-q",
    "M-q"
  ]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  insertPosition Below Newer
    <+> composeAll
      [ -- Firefox
        title =? "Mozilla Firefox" --> doShift (myWorkspaces !! 1),
        (className =? "Mozilla Firefox" <&&> resource =? "Dialog") --> doFloat,
        -- Spotify
        className =? "spotify" --> doShift (myWorkspaces !! 3),
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
            terminal = myTerminal,
            focusedBorderColor = "#b16286",
            normalBorderColor = "#ebdbb2",
            layoutHook = myLayoutHook,
            startupHook = myStartupHook,
            workspaces = myWorkspaces,
            manageHook = myManageHook,
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
          `removeKeysP` myRemKeys
          `additionalKeysP` myKeys
