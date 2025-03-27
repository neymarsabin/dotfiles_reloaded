#!/usr/bin/env sh
# define the fzf command: ./.fzfdir.sh pet
# syntax: ./.fzfdir.sh <working_directory>
# NOTE: this script does not work with directories with special characters
#
# FZF_COMMAND="fzf-tmux"
FZF_COMMAND="fzf-tmux -p --with-nth 1"

# find in directories
workdir=$1
base_dir=~/bucks
find_dir="$base_dir/*/*"

# Execute Command
searched_dir=$(ls -d $find_dir | $FZF_COMMAND)
RESULT=$(echo "$searched_dir" | sed 's#.*/##')

# Do nothing if result is EMPTY
# Works in case if you do not find something and press ESC
if [[ -n "$RESULT" ]]; then
    # create a new tmux session and attach to it
    window_name="server"
    session_name="$RESULT"
    workdir="$base_dir/$1/$RESULT"

    if ! tmux has-session -t "$session_name" 2>/dev/null; then
      tmux new-session -d -s "$session_name" -n "$window_name" -c "$searched_dir"
    fi

    tmux switch-client -t "$session_name"
fi

