#!/bin/bash
# This script takes the bookmarks from NERDTree and makes a dmenu choice menu out of these bookmarks.
# The input file can be changed but every line needs to be in the following format:
# [name] [path]\n
terminal="alacritty"
editor="nvim"
input=$HOME/.NERDTreeBookmarks
output=$(grep '' $input | cut -d ' ' -f 1)
choice=$(echo -e "${output[@]}" | dmenu -i -p 'Choose a bookmark: ')
path=$(grep -P "^${choice[@]}\s" $input | cut -d ' ' -f 2)
fixedPath=${path/'~'/$HOME}

if [ -d "${fixedPath}" ]
    then $terminal -e $editor -c ":cd "$fixedPath""
elif [ -f "${fixedPath}" ]
    then $terminal -e $editor "$fixedPath" -c ""
fi
