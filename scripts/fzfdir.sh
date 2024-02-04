#!/usr/bin/env sh
# Use at your own risk: I have seen bugs that create infinite tmux sessions :D
# define the fzf command: ./.fzfdir.sh pet
# syntax: ./.fzfdir.sh <working_directory>
# note: I have base_dir as ~/projects because I have all of my projects in ~/projects
# FZF_COMMAND="fzf-tmux"
FZF_COMMAND="fzf-tmux -p --with-nth 1"

# find in directories
workdir=$1
base_dir=~/projects
find_dir=$base_dir/$1

# Execute Command
RESULT=$(ls $find_dir | $FZF_COMMAND)

# Do nothing if result is EMPTY
# Works in case if you do not find something and press ESC
if [[ -z "$RESULT" ]]; then
   exit 1
fi

# create a new tmux session and attach to it
window_name=$RESULT
session_name="neymarsabin/$window_name"
workdir=$find_dir/$RESULT
send_command="cd $workdir"

if ! tmux has-session -t $session_name 2>/dev/null; then
  ## create new session, provide SESSION and WINDOW name
  new_session=$(TMUX= tmux new-session -A -d -s $session_name -n $window_name)

  ## switch and cd into project
  tmux switch-client -t $session_name
  tmux send-keys -t $session_name:$window_name "$send_command; clear" C-m
else
  tmux switch-client -t $session_name
fi

