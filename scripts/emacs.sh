#!/usr/bin/env bash
###emacs le yeei khanxa aba dekhi
#copy init.el file and cask from here
cp $config_dir/emacs/* ~/.emacs.d/

### cask reload garna laai yo hai 
function cask_reload {
		typeset current_dir=$PWD
    cd ~/.emacs.d/
		cask --verbose
		cd $current_dir
}

alias eamcs='emacs'
alias emasc='emacs'
alias emcas='emacs'
alias emcsa='emacs'
alias meacs='emacs'

# alias to emacsclient
alias ecs='emacsclient'
