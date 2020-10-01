# `plug.kak` Configuration
#
# This script configures plugins to install to `$XDG_CACHE_HOME` by default
# instead of the Kakoune configuration directory and adds code to upgrade
# `plug.kak`.

source "%val{config}/plugins/plug.kak"
set-option global plug_install_dir %sh{
    echo "${XDG_CACHE_HOME:-$HOME/.cache}/kak/plugins"
}

define-command -override -docstring \
"plug-upgrade: update plug.kak." \
plug-upgrade %{ evaluate-commands %sh{
    echo 'echo "Upgrading plug.kak..."'
    output=$(curl --no-progress-meter -fL \
        "https://raw.githubusercontent.com/robertmeta/plug.kak/master/rc/plug.kak" \
        -o "$kak_config/plugins/plug.kak" \
        2>&1 \
    )
    if [ $? -ne 0 ]; then
        echo 'echo "Error updating plug.kak"'
        echo 'echo -debug "'"$output"'"'
        exit 1
    fi

    echo 'echo "Done"'
}}
