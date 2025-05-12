#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
HOME_DIR=$( realpath ~ )

LN=$( which ln )
if [ ! $LD -n ]; then
    echo Could not locate ln. Exiting.
    exit
fi

declare -a dotfiles=(
    ".emacs.d/init.el"
)

for dotfile in "${dotfiles[@]}"
do
    [ ! -e "$HOME_DIR/$dotfile" ]
    target_exists=$?
    
    [ ! -e "$SCRIPT_DIR/$dotfile" ]
    source_exists=$?

    if [ $source_exists -eq 0 ]; then
	echo "Error: source file \"$dotfile\" not found in repository."
	continue
    fi

    result=0
    while [ "$result" != "y" ] && [ "$result" != "n" ] && [ "$result" != "q" ];
    do
	if [ $target_exists -eq 0 ]; then
	    prompt="Create \"~/$dotfile\" as symlink to \"$SCRIPT_DIR/$dotfile\"? (y/n/q)"
	else
	    prompt="Overwrite \"~/$dotfile\" with symlink to \"$SCRIPT_DIR/$dotfile\"? (y/n/q)"
	fi
	echo -n "$prompt "
	read result
    done

    if [ "$result" = "q" ]; then
	exit
    fi
    if [ "$result" = "n" ]; then
	continue
    fi

    $LN -sf $SCRIPT_DIR/$dotfile $HOME_DIR/$dotfile
done

