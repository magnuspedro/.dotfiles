ALACRITTY_DIR="/home/$USER/.config/alacritty"
TMP_DIR="~/.tmux/plugins/tmp"
WOFI_DIR="/home/$USER/.config/wofi"
SWAY_DIR="/home/$USER/.config/sway"
TMUX_DIR="/home/$USER/.config/tmux"


[ ! -d "$ALACRITTY_DIR" ] && ln -s /home/$USER/.dotfiles/alacritty /home/$USER/.config/alacritty
[ ! -d "$WOFI_DIR" ] && ln -s /home/$USER/.dotfiles/wofi /home/$USER/.config/wofi
[ ! -d "$SWAY_DIR" ] && ln -s /home/$USER/.dotfiles/sway /home/$USER/.config/sway
[ ! -d "$TMUX_DIR" ] && ln -s /home/$USER/.dotfiles/tmux /home/$USER/.config/tmux


[ ! -d "$TMP_DIR" ] &&  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
