#!/usr/bin/env bash

### copy the i3 configuration file to the home folder
cp $config_dir/i3config ~/.config/i3/config 

# always load i3 from a config file and stuffs
alias i3='i3 -c $config_dir/i3config'
