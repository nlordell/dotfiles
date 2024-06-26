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

gitinst() {
	local remote="https://github.com/$1"
	local dst="$HOME/$2"

	if [ ! -e "$dst" ]; then
		git clone "$remote" "$dst"
	else
		echo "skipping $1: '$2' already exists." >&2
	fi
}

mkdir -p "$HOME/.config"

dotlink "config/emacs"
dotlink "config/kak"
dotlink "config/nvim"
gitinst \
	"wbthomason/packer.nvim" \
	".local/share/nvim/site/pack/packer/start/packer.nvim"

if which nvim &>/dev/null; then
    nvim --headless -c "autocmd User PackerComplete quitall" -c "PackerSync"
fi
