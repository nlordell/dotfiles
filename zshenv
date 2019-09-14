# Configure ZSH to run from ZDOTDIR, this the first user script that is sourced
# by ZSH regardless of how the shell is started (login, interactive, etc.). Also
# make sure to source the ZDOTDIR/.zshenv file where we actually set up our user
# environment, the other ZSH user scripts get sourced automagically.

ZDOTDIR="$HOME/.config/zsh"
. "$ZDOTDIR/.zshenv"
