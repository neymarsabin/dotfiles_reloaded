#!/usr/bin/env sh
# only for first time installations in laptops
# existing system can use sudo pacman -Syu

packages=(
    "alacritty"
    "bc"
    "blueman"
    "bluez"
    "bluez-utils"
    "docker"
    "docker-compose"
    "dunst"
    "emacs-nativecomp"
    "font-manager"
    "fzf"
    "go"
    "greenclip"
    "helvum"
    "hugo"
    "jq"
    "k9s"
    "kubectl"
    "man"
    "mplayer"
	"nvtop"
    "pavucontrol"
    "pdflatex"
    "pipewire-audio"
    "pipewire-media-session"
    "pipewire-pulse"
    "postgresql-libs"
    "ranger"
    "ripgrep"
    "rofi"
    "telegram-desktop"
    "ttf-Jetbrains-mono"
    "ttf-dejavu"
    "ttf-font-awesome"
    "ttf-monaco"
    "unzip"
    "vlc"
    "xbindkeys"
    "yaourt"
)

# install packages one by one
sudo pacman -Syu
for package in ${packages[@]}; do
    # --needed skips reinstall if the package already exists
    # I love pacman, there are options for anything you need of
    sudo pacman -S --needed $package
done
