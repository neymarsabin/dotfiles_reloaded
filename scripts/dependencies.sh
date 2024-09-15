#!/usr/bin/env sh
# only for first time installations in laptops
# existing system can use sudo pacman -Syu

packages=(
    "bc"
    "blueman"
    "bluez"
    "bluez-utils"
    "docker"
    "docker-compose"
    "dunst"
    "fzf"
    "greenclip"
    "hugo"
    "jq"
    "man"
    "mplayer"
    "nvtop"
    "pavucontrol"
    "pdflatex"
    "pipewire-audio"
    "pipewire-media-session"
    "pipewire-pulse"
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
)

# install packages one by one
sudo pacman -Syu
for package in ${packages[@]}; do
    # --needed skips reinstall if the package already exists
    # I love pacman, there are options for anything you need of
    sudo pacman -S --needed $package
done
