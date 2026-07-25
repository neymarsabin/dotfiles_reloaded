#!/bin/bash
# Launch the fzf picker in a floating alacritty popup, then focus the selected
# window AFTER alacritty exits (prevents macOS from stealing focus back).

TMPFILE="/tmp/aerospace-switcher.tmp"
LOGFILE="/tmp/aerospace-switcher.log"
rm -f "$TMPFILE"
echo "launcher started at $(date)" > "$LOGFILE"

/Users/neymarsabin/.cargo/bin/alacritty \
  -q \
  -T aerospace-switcher \
  -o "window.dimensions.columns=90" \
  -o "window.dimensions.lines=15" \
  -e /Users/neymarsabin/projects/pet/dotfiles_reloaded/dots/scripts/window-switcher.sh

# Focus runs after alacritty has fully closed
echo "alacritty exited" >> "$LOGFILE"
if [ -f "$TMPFILE" ]; then
  window_id=$(cat "$TMPFILE")
  echo "focusing window_id: '$window_id'" >> "$LOGFILE"
  rm -f "$TMPFILE"
  sleep 0.15
  /opt/homebrew/bin/aerospace focus --window-id "$window_id" >> "$LOGFILE" 2>&1
  echo "focus exit code: $?" >> "$LOGFILE"
else
  echo "tmpfile not found" >> "$LOGFILE"
fi
