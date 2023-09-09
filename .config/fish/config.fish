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
alias .="pwd"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias lsf="ls -p | grep -v /"
alias llf="ls -lp | grep -v /"
alias sudo='sudo '
alias g="git"
alias lg="lazygit"
alias z="zoxide"
alias emacs="emacs -mm"
alias rsync="rsync -h --progress"
alias netspeed="curl -s https://raw.githubusercontent.com/sivel/speedtest-cli/master/speedtest.py | python -"
alias dockvimgolf='docker run --rm -it -e "key=0da1a0709b69c31106b2c8a7900ce6f5" ghcr.io/filbranden/vimgolf'
alias gap="git add -p"
alias mpi="mpv --keep-open=yes"

# aliases for scripts
alias gas="~/.dotfiles/.scripts/git_auto_sync.sh"

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

# doom emacs
set -x PATH "$HOME/.config/emacs/bin:$PATH"

# calibre
set -Ux CALIBRE_USE_DARK_PALETTE 1

# pyenv
#pyenv init - | source

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

# chmod 755 ~/.dotfiles/.scripts/*
# bash ~/.dotfiles/.scripts/autostart.sh

# tabtab source for packages
# uninstall by removing these lines
[ -f ~/.config/tabtab/fish/__tabtab.fish ]; and . ~/.config/tabtab/fish/__tabtab.fish; or true
