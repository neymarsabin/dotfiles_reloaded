#!/usr/bin/env sh
todays_date=$(date +%Y-%m-%d-%H-%M-%S)
scrot -s $HOME/screenshots/$todays_date.jpg
