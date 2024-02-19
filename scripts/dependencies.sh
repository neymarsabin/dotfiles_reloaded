#!/usr/bin/env sh
# only for first time installations in laptops
# existing system can use sudo pacman -Syu

packages=(
    "emacs-nativecomp"
    "ttf-font-awesome"
    "rofi"
    "helvum"
    "pipewire-audio"
    "pipewire-pulse"
    "pavucontrol"
    "pipewire-media-session"
    "alacritty"
    "jq"
    "ttf-monaco"
    "yaourt"
    "ttf-Jetbrains-mono"
    "ttf-dejavu"
    "font-manager"
    "fzf"
    "go"
    "dunst"
    "man"
    "docker"
    "docker-compose"
    "telegram-desktop"
    "ranger"
    "hugo"
    "pdflatex"
    "vlc"
    "mplayer"
    "bluez"
    "bluez-utils"
    "blueman"
    "xbindkeys"
    "bc"
    "kubectl"
    "k9s"
    "greenclip"
    "postgresql-libs"
    "ripgrep"
    "unzip"
)

# install packages one by one
sudo pacman -Syu
for package in ${packages[@]}; do
    # --needed skips reinstall if the package already exists
    # I love pacman, there are options for anything you need of
    sudo pacman -S --needed $package
done
