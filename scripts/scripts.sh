#!/usr/bin/env bash

function play_yt() {
		youtube-dl -q -o- $1 | mplayer -cache 8192  -
}

function screen_capture {
		todays_date=$(date +%Y%m%d%H%M%S)
		import $HOME/screenshots/$todays_date.jpg
}

function gear_up() {
		curl --header "Authorization: Bearer ${PUSH_BULLET_API_TOKEN}" -X POST https://api.pushbullet.com/v2/pushes --header 'Content-Type: application/json' --data-binary "{'type': $1, 'title': $2, 'body': $3}"
}
