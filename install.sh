#!/usr/bin/env bash

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

nvimpack() {
	echo "$HOME/.local/share/nvim/site/pack/$1/start/$2"
}

gitinst() {
	if [[ ! -e "$2" ]]; then
		local remote="git@github.com:$1.git"
		git clone "$remote" "$2"
	fi
}

mkdir -p "$HOME/.config"

dotlink "config/nvim"
dotlink "config/kak"

gitinst \
	"wbthomason/packer.nvim" \
	"$(nvimpack "packer" "packer.nvim")"

nvim +PackerInstall +PackerCompile +qall
