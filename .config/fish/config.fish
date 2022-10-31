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
alias lg="lazygit"

# starship
starship init fish | source

# Setting PATH for Python 3.10
# The original version is saved in /Users/sumanthyedoti/.config/fish/config.fish.pysave
set -x PATH "/Library/Frameworks/Python.framework/Versions/3.10/bin" "$PATH"
