#!/bin/bash
# Rofi-like window switcher using aerospace + fzf
# Writes selected window-id to a temp file; focus happens after alacritty exits.

TMPFILE="/tmp/aerospace-switcher.tmp"

selected=$(
  /opt/homebrew/bin/aerospace list-windows --all --format '%{window-id} | %{app-name} | %{window-title}' \
    | /opt/homebrew/bin/fzf \
        --prompt='  ' \
        --layout=reverse \
        --border=rounded \
        --height=100% \
        --color='bg:#1e1e2e,bg+:#313244,fg:#cdd6f4,fg+:#cdd6f4' \
        --color='border:#89b4fa,prompt:#cba6f7,pointer:#f5e0dc' \
        --color='hl:#f38ba8,hl+:#f38ba8'
)

[ -z "$selected" ] && exit 0

window_id=$(echo "$selected" | awk '{print $1}')
echo "$window_id" > "$TMPFILE"
