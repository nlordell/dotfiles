#!/bin/bash

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null && pwd)"

dotlink() {
    local src="$ROOT/$1"
    if [ -z "$2" ]; then
        local dst="$HOME/.$1"
    else
        local dst="$HOME/$2"
    fi

    if [ ! -e "$dst" ]; then
        ln -s "$src" "$dst"
        echo "linked '$src' to '$dst'"
    else
        echo "skipping $1: '$dst' already exists." >&2
    fi
}

mkdir -p "$HOME/.config"

dotlink "config/nvim"
dotlink "config/sway"

nvim +PlugInstall +qall
