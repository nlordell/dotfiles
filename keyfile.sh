#!/usr/bin/env bash

set -euo pipefail

mkdir -p "$HOME/.local/share/keepass"

# We use the SLIP-0016 master key as the key file for our KeePass database,
# with a more appropriate key.
# <https://github.com/satoshilabs/slips/blob/master/slip-0016.md>

ADDRESS=m/10016h/0
KEY='Unlock vault.kdbx?'
VALUE='2d650551248d792eabf628f451200d7f51cb63e46aadcbb1038aacb05e8c8aee2d650551248d792eabf628f451200d7f51cb63e46aadcbb1038aacb05e8c8aee'

"${TREZORCTL:-trezorctl}" encrypt-keyvalue --address "$ADDRESS" --prompt always "$KEY" "$VALUE" \
	| xxd -r -p > "$HOME/.local/share/keepass/trezor.key"
