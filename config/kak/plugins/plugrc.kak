# `plug.kak` Configuration
#
# This script configures plugins to install to `$XDG_CACHE_HOME` by default
# instead of the Kakoune configuration directory and adds code to upgrade
# the checked-in `plug.kak`.

source "%val{config}/plugins/plug.kak"
set-option global plug_install_dir %sh{
    echo "${XDG_CACHE_HOME:-$HOME/.cache}/kak/plugins"
}

plug "robertmeta/plug.kak" noload do %{
    kak_config="${XDG_CONFIG_HOME:-$HOME/.config}/kak"
    cp rc/plug.kak "$kak_config/plugins/plug.kak"
}
