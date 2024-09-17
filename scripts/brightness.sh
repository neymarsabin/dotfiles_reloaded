#!/usr/bin/env sh
### dependecies: xrandr, bc, xargs, cut
### control brightness with xrandr
### display name
output=eDP

## find current brightness level using xrandr
current_brightness=$(xrandr --verbose | grep Brightness | xargs | cut -d ' ' -f2)

## if parameter is inc
type=$1

## inc brightness if type == "inc"
if [[ $type == "inc" ]]; then
    new_brightness=$(bc -l <<<"${current_brightness}+0.1")
    xrandr --output $output --brightness $new_brightness
fi

## dec brightness if type == "dec"
if [[ $type == "dec" ]]; then
    new_brightness=$(bc -l <<<"${current_brightness}-0.1")
    xrandr --output $output --brightness $new_brightness
fi
