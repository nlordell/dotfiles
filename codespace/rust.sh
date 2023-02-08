#!/usr/bin/env bash

if [ "$CODESPACES" != "true" ]; then
	echo "ERROR: not in a codespace." >&2
	exit 1
fi

curl -s https://sh.rustup.rs | sh -s -- -y --component rust-analyzer

# For some reason, rustup does not create symbolic links to the rust-analyzer
# binary, so create one.
source "$HOME/.cargo/env"
ln -s "$(rustup which rust-analyzer)" ~/.local/bin/rust-analyzer
