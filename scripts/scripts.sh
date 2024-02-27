#!/usr/bin/env bash

function screen_capture {
		todays_date=$(date +%Y%m%d%H%M%S)
		import $HOME/screenshots/$todays_date.jpg
}
