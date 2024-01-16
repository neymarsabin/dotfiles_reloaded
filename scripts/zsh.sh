#!/usr/bin/env bash

#for my shell
alias ..='cd ..'
alias ...= 'cd ../../'

# screenfetch
# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# copy command alias to set interactive copy
alias cp='cp -i -v '
# remove command made interactive
alias rm='rm -i'
#mv command with interactive
alias mv='mv -i'

# some git aliases
alias glol='git log --oneline --all --graph --decorate'

# I am on macOS these days, group these with comments
# for gpu settings
# xrandr --setprovideroffloadsink 1 0
# DRI_PRIME=1 glxinfo 1> /dev/null

# start ibus for language selection
# ibus-daemon -drx
#
#### some environment variables #####
# environment variables
# export PATH=$PATH:/home/neymar/Android/Sdk/tools
# export ANDROID_HOME=/home/neymar/Android/Sdk

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

## fixing go binary path
export PATH=$PATH:/usr/local/go/bin

# alias ls into exa
alias ls='exa -lhSHgi --git'

echo 'ulimit -n 12288' >> ~/.bash_profile
export PATH=$PATH:/Users/neymarsabin/Documents/mongo/bin
export PATH="/opt/homebrew/opt/mongodb-community@4.4/bin:$PATH"

# chatgpt command line interface
export PATH=$PATH:/Users/neymarsabin/.config/emacs/bin

# add golang path to emacs
export GOPATH="$(go env GOPATH)"
export PATH="${PATH}:${GOPATH}/bin"

# load doom config from dotfiles repo, !home folder
dotfile_location=projects/pet/dotfiles_reloaded
export DOOMDIR=$HOME/$dotfile_location/dots/doom
