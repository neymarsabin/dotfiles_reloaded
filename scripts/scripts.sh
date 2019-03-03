#!/usr/bin/env bash

function play_yt() {
		youtube-dl -q -o- $1 | mplayer -cache 8192  -
}

function screen_capture {
		todays_date=$(date +%Y%m%d%H%M%S)
		import ~/screenshots/$todays_date.jpg
}
