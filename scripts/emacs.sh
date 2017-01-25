#!/usr/bin/env bash
###emacs le yeei khanxa aba dekhi
#copy init.el file and cask from here
cp $config_dir/emacs/* ~/.emacs.d/

### cask reload garna laai yo hai 
function cask_reload {
    cd ~/.emacs.d/
		cask
		cd 
}

# j gareni emacs//// from milan dai's config
alias eamcs='emacs'
alias emasc='emacs'
alias emcas='emacs'
alias emcsa='emacs'
alias meacs='emacs'
