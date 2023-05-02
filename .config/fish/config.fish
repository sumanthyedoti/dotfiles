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
# rust/cargo
set PATH $HOME/.cargo/bin $PATH

# Andorind Studio
set -x ANDROID_SDK_ROOT "$HOME/Library/Android/sdk"
set -x PATH "$PATH:$ANDROID_SDK_ROOT/emulator"
set -x PATH "$PATH:$ANDROID_SDK_ROOT/platform-tools"
# pyenv
pyenv init - | source

#### emacs
# vterm
function vterm_printf;
    if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end
        # tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end
# vterm clear scrollback
if [ "$INSIDE_EMACS" = 'vterm' ]
    function clear
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    end
end
