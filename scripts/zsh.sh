#!/usr/bin/env bash

#for my shell

###### some aliases #################

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

# bundle to b
alias b='bundle'
alias be='bundle exec'



### some environment variables #####

# environment variables
# export PATH=$PATH:/home/neymar/Android/Sdk/tools
# export ANDROID_HOME=/home/neymar/Android/Sdk

# for laravel 
# export PATH=$PATH:/home/neymar/.config/composer/vendor/bin

#for emacs dependency manager cask
export PATH="/home/neymar/.cask/bin:$PATH"


# my configs directory
export config_dir="/mnt/hackit/codeds/dotfiles/dots"

############ some useful functions #############

#run php server 
function php-server {
    php -S localhost:8000 -t $1
}

