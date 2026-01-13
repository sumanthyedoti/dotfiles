# Mac config
if [[ $(uname) == "Darwin" ]]; then
  # Set PATH, MANPATH, etc., for Homebrew.
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

export PATH=$PATH:/home/$USER/.config/emacs

export ANDROID_HOME=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/platform-tools


# Herd injected PHP binary.
export PATH="/Users/sumanthyedoti/Library/Application Support/Herd/bin/":$PATH


# Herd injected PHP 8.4 configuration.
export HERD_PHP_84_INI_SCAN_DIR="/Users/sumanthyedoti/Library/Application Support/Herd/config/php/84/"


# Herd injected PHP 8.2 configuration.
export HERD_PHP_82_INI_SCAN_DIR="/Users/sumanthyedoti/Library/Application Support/Herd/config/php/82/"

. "$HOME/.local/bin/env"
