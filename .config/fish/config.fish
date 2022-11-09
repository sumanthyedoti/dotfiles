fish_vi_key_bindings
if status is-interactive
    # Commands to run in interactive sessions can go here
end

# mkdir and cd
function mkcd
    mkdir -p -- $argv && cd -- $argv
end

# Mac config
if test (uname) = "Darwin"
  # Set PATH, MANPATH, etc., for Homebrew.
  eval (/opt/homebrew/bin/brew shellenv)
end

# ALIASES
alias g="git"
alias lg="lazygit"
alias z="zoxide"

starship init fish | source
zoxide init fish | source

# Setting PATH for Python 3.10
# The original version is saved in /Users/sumanthyedoti/.config/fish/config.fish.pysave
set -x PATH "/Library/Frameworks/Python.framework/Versions/3.10/bin" "$PATH"

set -x PATH "/Library/Frameworks/Python.framework/Versions/3.10/bin" "$PATH"
# Andorind Studio
set -x ANDROID_SDK_ROOT "$HOME/Library/Android/sdk"
set -x PATH "$PATH:$ANDROID_SDK_ROOT/emulator"
set -x PATH "$PATH:$ANDROID_SDK_ROOT/platform-tools"
# pyenv
pyenv init - | source
