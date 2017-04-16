#!/usr/bin/env bash

# start tmux from here using the config file

alias tmux='tmux -f $config_dir/tmux'

# started using ranger somedays ago,start a spearate tmux session with ranger and detach from it automatically
function tmux_ranger {
		typeset current=$PWD
		cd 
		tmux new -s ranger -d 
		cd $current
}




