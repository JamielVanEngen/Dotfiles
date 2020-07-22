#!/bin/bash
output=$(grep '' $HOME/.NERDTreeBookmarks | cut -d ' ' -f 1)
choice=$(echo -e "${output[@]}" | dmenu -i -p 'Choose a bookmark: ')
path=$(grep -P "^${choice[@]}\s" $HOME/.NERDTreeBookmarks | cut -d ' ' -f 2)
fixedPath=${path/'~'/$HOME}
terminal="alacritty"
editor="nvim"

if [ -d "${fixedPath}" ]
    then $terminal -e $editor -c ":cd "$fixedPath""
elif [ -f "${fixedPath}" ]
    then $terminal -e $editor "$fixedPath" -c ""
fi
