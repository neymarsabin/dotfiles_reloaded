#!/usr/bin/env bash

#for my shell

###### some aliases #################

# alias for Discord app
alias discord='/mnt/hackit/codeds/Discord/Discord'

#basic navigation of my folders to aliases 
alias codeds='cd /mnt/hackit/codeds ' 
alias suta='sudo systemctl start suspend.target'
alias ngrok='/mnt/hackit/everything/ngrok/ngrok'
alias hackit='/mnt/hackit/'

#back made easy
alias ..='cd ..'
alias ...= 'cd ../../'

# screenfetch 
# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# copy command alias to set interactive copy
alias cp='cp -i'
# remove command made interactive
alias rm='rm -i'
#mv command with interactive
alias mv='mv -i'
# bundle to b
alias b='bundle'
alias be='bundle exec'


# some git aliases
alias glol='git log --oneline --all --graph --decorate'
# power options laptop
alias gotosleep='sudo systemctl suspend'
alias gotodie='shutdown -h'
alias refresh='reboot'


### some environment variables #####

# environment variables
# export PATH=$PATH:/home/neymar/Android/Sdk/tools
# export ANDROID_HOME=/home/neymar/Android/Sdk

# for laravel 
# export PATH=$PATH:/home/neymar/.config/composer/vendor/bin

#for emacs dependency manager cask
export PATH="/home/neymar/.cask/bin:$PATH"
export EDITOR="emacsclient -nw"
# exporting path for jruby exectuionals
export PATH="/mnt/hackit/codeds/jruby/jruby-9.1.8.0/bin:$PATH"

#for rvm and stuffs


# my configs directory
export config_dir="/mnt/hackit/codeds/dotfiles/dots"


############ some useful functions #############
function mount_usb {
		sudo mount -o gid=users,fmask=113,dmask=002 $1 /mnt/pendrive_is_on_high
}

function umount_usb {
    sudo umount /mnt/pendrive_is_on_high
}


