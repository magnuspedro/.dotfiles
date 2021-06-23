from libqtile.lazy import lazy
from libqtile.config import Key
from env import mod, my_term


keys = [
         ### The essentials
         Key([mod], "Return",
             lazy.spawn(my_term),
             desc='Launches My Terminal'
             ),
         Key([mod], "r",
             lazy.spawn("dmenu_run -p 'Run: '"),
             desc='Run Launcher'
             ),
         Key([mod], "Tab",
             lazy.next_layout(),
             desc='Toggle through layouts'
             ),
         Key([mod, "shift"], "c",
             lazy.window.kill(),
             desc='Kill active window'
             ),
         Key([mod, "shift"], "r",
             lazy.restart(),
             desc='Restart Qtile'
             ),
         Key([mod, "shift"], "q",
             lazy.shutdown(),
             desc='Shutdown Qtile'
             ),
         ### Switch focus to specific monitor (out of three)
         Key([mod], "w",
             lazy.to_screen(0),
             desc='Keyboard focus to monitor 1'
             ),
         Key([mod], "e",
             lazy.to_screen(1),
             desc='Keyboard focus to monitor 2'
             ),
         ### Switch focus of monitors
         Key([mod], "period",
             lazy.next_screen(),
             desc='Move focus to next monitor'
             ),
         Key([mod], "comma",
             lazy.prev_screen(),
             desc='Move focus to prev monitor'
             ),
         ### Treetab controls
          Key([mod, "shift"], "h",
             lazy.layout.move_left(),
             desc='Move up a section in treetab'
             ),
         Key([mod, "shift"], "l",
             lazy.layout.move_right(),
             desc='Move down a section in treetab'
             ),
         ### Window controls
         Key([mod], "j",
             lazy.layout.down(),
             desc='Move focus down in current stack pane'
             ),
         Key([mod], "k",
             lazy.layout.up(),
             desc='Move focus up in current stack pane'
             ),
         Key([mod, "shift"], "j",
             lazy.layout.shuffle_up(),
             lazy.layout.section_up(),
             desc='Move windows down in current stack'
             ),
         Key([mod, "shift"], "k",
             lazy.layout.shuffle_down(),
             lazy.layout.section_down(),
             desc='Move windows up in current stack'
             ),
         Key([mod], "h",
             lazy.layout.shrink(),
             lazy.layout.decrease_nmaster(),
             desc='Shrink window (MonadTall), decrease number in master pane (Tile)'
             ),
         Key([mod], "l",
             lazy.layout.grow(),
             lazy.layout.increase_nmaster(),
             desc='Expand window (MonadTall), increase number in master pane (Tile)'
             ),
         Key([mod], "n",
             lazy.layout.normalize(),
             desc='normalize window size ratios'
             ),
         Key([mod], "m",
             lazy.layout.maximize(),
             desc='toggle window between minimum and maximum sizes'
             ),
         Key([mod, "shift"], "f",
             lazy.window.toggle_floating(),
             desc='toggle floating'
             ),
         Key([mod], "f",
             lazy.window.toggle_fullscreen(),
             desc='toggle fullscreen'
             ),
         ### Stack controls
         Key([mod, "shift"], "Tab",
             lazy.layout.rotate(),
             lazy.layout.flip(),
             desc='Switch which side main pane occupies (XmonadTall)'
             ),
          Key([mod], "space",
             lazy.layout.next(),
             desc='Switch window focus to other pane(s) of stack'
             ),
          Key([mod, "shift"], "space",
             lazy.layout.toggle_split(),
             desc='Toggle between split and unsplit sides of stack'
             ),
         # Change the volume if your keyboard has special volume keys.
         Key(
             [], "XF86AudioRaiseVolume",
             lazy.spawn("amixer -c 0 -q set Master 2dB+")
         ),
         Key(
             [], "XF86AudioLowerVolume",
             lazy.spawn("amixer -c 0 -q set Master 2dB-")
         ),
         Key(
             [], "XF86AudioMute",
             lazy.spawn("amixer -c 0 -q set Master toggle")
         ),
         # Key(
             # [], "XF86AudioPlay",
             # lazy.function(playpause)
         # ),
         # Key(
             # [], "XF86AudioNext",
             # lazy.function(next_prev("next"))
         # ),
         # Key(
             # [], "XF86AudioPrev",
             # lazy.function(next_prev("prev"))
         # ),

         # Also allow changing volume the old fashioned way.
         Key([mod], "equal", lazy.spawn("amixer -c 0 -q set Master 2dB+")),
         Key([mod], "minus", lazy.spawn("amixer -c 0 -q set Master 2dB-")),
         # backlight controls
         # Key([], "XF86KbdBrightnessUp", lazy.spawn("maclight keyboard up")),
         # Key([], "XF86KbdBrightnessDown", lazy.spawn("maclight keyboard down")),
         Key([], "XF86MonBrightnessUp", lazy.spawn("xbacklight -inc 10")),
         Key([], "XF86MonBrightnessDown", lazy.spawn("xbacklight -dec 10")),
         # Printscreen
         Key([], "Print", lazy.spawn("scrot -s 'screenshot-%Y-%m-%d-%s_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f' -e 'mv $f /home/magnus/Pictures/Screenshots'")),
         # Set 2 monitors 
         Key([mod], "p",
             lazy.spawn("xrandr --output eDP1 --mode 1366x768 --pos 0x0 --rotate normal --right-of HDMI1 --output HDMI1 --mode 1600x900 --pos 0x0 --rotate normal --right-of eDP1 --output HDMI2 --off"),
             lazy.restart()
             )
         # Dmenu scripts launched using the key chord SUPER+p followed by 'key'
         # KeyChord([mod], "p", [
             # Key([], "e",
                 # lazy.spawn("./dmscripts/dm-confedit"),
                 # desc='Choose a config file to edit'
                 # ),
             # Key([], "i",
                 # lazy.spawn("./dmscripts/dm-maim"),
                 # desc='Take screenshots via dmenu'
                 # ),
             # Key([], "k",
                 # lazy.spawn("./dmscripts/dm-kill"),
                 # desc='Kill processes via dmenu'
                 # ),
             # Key([], "l",
                 # lazy.spawn("./dmscripts/dm-logout"),
                 # desc='A logout menu'
                 # ),
             # Key([], "m",
                 # lazy.spawn("./dmscripts/dm-man"),
                 # desc='Search manpages in dmenu'
                 # ),
             # Key([], "o",
                 # lazy.spawn("./dmscripts/dm-bookman"),
                 # desc='Search your qutebrowser bookmarks and quickmarks'
                 # ),
             # Key([], "r",
                 # lazy.spawn("./dmscripts/dm-reddit"),
                 # desc='Search reddit via dmenu'
                 # ),
             # Key([], "s",
                 # lazy.spawn("./dmscripts/dm-websearch"),
                 # desc='Search various search engines via dmenu'
                 # ),
             # Key([], "p",
                 # lazy.spawn("passmenu"),
                 # desc='Retrieve passwords with dmenu'
                 # )
         # ])
]
