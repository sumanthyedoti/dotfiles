# Mac config
if [[ $(uname) == "Darwin" ]]; then
  # Set PATH, MANPATH, etc., for Homebrew.
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi
. "$HOME/.cargo/env"

export PATH=$PATH:/home/$USER/.config/emacs
