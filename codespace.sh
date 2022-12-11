#!/usr/bin/env bash

if [ "$CODESPACES" != "true" ]; then
	echo "ERROR: not in a codespace." >&2
	exit 1
fi

if [ ! -x "$(command -v gh)" ]; then
	echo "ERROR: missing \`gh\` command." >&2
	exit 1
fi

instopt() {
	local repo="$1"; shift
	local bin="$1"; shift
	local pat="$1"; shift

	local name="$(basename "$repo")"
	echo "### $name ###"
	mkdir -p "/opt/$name"
	pushd "/opt/$name" >/dev/null

	gh release download -R "$repo" -p "$pat" --clobber
	if [[ "$pat" =~ \.zip$ ]]; then
		unzip -o $pat "$@" >/dev/null
	else
		tar -xvf $pat "$@" >/dev/null
	fi

	local linkname="$HOME/.local/bin/$(basename "$bin")"
	if [[ ! -e "$linkname" ]]; then
		ln -s "$(pwd)/$bin" "$linkname"
	fi

	popd >/dev/null
}

instopt burntsushi/ripgrep \
	rg '*-x86_64-unknown-linux-musl.tar.gz' --strip-components=1
instopt denoland/deno \
	deno '*-x86_64-unknown-linux-gnu.zip'
instopt junegunn/fzf \
	fzf '*-linux_amd64.tar.gz'
instopt neovim/neovim \
	bin/nvim '*-linux64.tar.gz' --strip-components=1
instopt sharkdp/bat \
	bat '*-x86_64-unknown-linux-gnu.tar.gz' --strip-components=1
instopt sharkdp/fd \
	fd '*-x86_64-unknown-linux-gnu.tar.gz' --strip-components=1
instopt zellij-org/zellij \
	zellij '*-x86_64-unknown-linux-musl.tar.gz'
