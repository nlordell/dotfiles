# Enable Wayland for Firefox
export MOZ_ENABLE_WAYLAND=1

# Backup settings
export RESTIC_REPOSITORY=/run/media/nlordell/Neptune/Backups/restic
export RESTIC_PASSWORD_COMMAND="secret-tool lookup setting-name backup backup-location '$RESTIC_REPOSITORY'"
