fish_vi_key_bindings
if status is-interactive
    # Commands to run in interactive sessions can go here
end

# mkdir and cd
function mkcd
    mkdir -p -- $argv && cd -- $argv
end

# Mac config
if test (uname) = Darwin
    # Set PATH, MANPATH, etc., for Homebrew.
    eval (/opt/homebrew/bin/brew shellenv)
end

# ALIASES
alias .="pwd"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

# `command` is needed in case `lfcd` is aliased to `lf`
alias gas="~/.dotfiles/.scripts/git_auto_sync.sh"
# aliases for scripts
alias txa="tmux attach"
alias txss="tmux new-session -d -s (pwd | sed 's/.*\///g') && tmux switch-client -t (pwd | sed 's/.*\///g')"
alias txnn="tmux new -s (pwd | sed 's/.*\///g')"
alias txn="tmux new"
alias mpi="mpv --keep-open=yes"
alias uuid="uuidgen | tr '[:upper:]' '[:lower:]'"
alias gap="git add -p"
alias no-nvidia-screen-tearing='nvidia-settings --assign CurrentMetaMode="nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"'
alias dockvimgolf='docker run --rm -it -e "key=0da1a0709b69c31106b2c8a7900ce6f5" ghcr.io/filbranden/vimgolf'
alias docker-ip="sudo docker inspect -f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}'"
alias netspeed="curl -s https://raw.githubusercontent.com/sivel/speedtest-cli/master/speedtest.py | python -"
alias rsync="rsync -h --progress"
alias emacs="emacs -mm"
alias http="xh"
alias m="mpv"
alias z="zoxide"
alias lg="lazygit"
alias g="git"
alias sudo='sudo '
alias lf="lfu ."
alias trm="trash-put"
alias lltr="exa -lF --tree --icons --git"
alias ltr="exa -F --tree --icons --git"
alias llt="exa -lF --level=2 --tree --icons --git"
alias lst="exa -F --level=2  --tree --icons --git"
alias llf="exa -lF --icons --git | grep -v /"
alias lsf="exa -1F --icons --git | grep -v /"
alias lla="exa -lF --icons --git -a"
alias ll="exa -lF --icons --git"
alias l="exa -F --icons"
alias clr="clear"
alias ....="cd ../../.."
alias ...="cd ../.."
alias ..="cd .."
alias .="pwd"
# ALIASES
alias clr="clear"

alias l="exa -F --icons"
alias ll="exa -lF --icons --git"
alias lla="exa -lF --icons --git -a"
alias lsf="exa -1F --icons --git | grep -v /"
alias llf="exa -lF --icons --git | grep -v /"
alias lst="exa -F --level=2  --tree --icons --git"
alias llt="exa -lF --level=2 --tree --icons --git"
alias ltr="exa -F --tree --icons --git"
alias lltr="exa -lF --tree --icons --git"

alias fz="fzf --height 30%"
alias fzp='fzf --preview "bat --style numbers,changes --color=always {} | head -200"' ## with preview

alias lf="lfu ."

alias sudo='sudo '
alias g="git"
alias d="docker"
alias dc="docker compose"
alias p="pnpm"
alias y="yarn"
alias lg="lazygit"
alias z="zoxide"
alias m="mpv"
alias http="xh"
alias emacs="emacs -mm"
alias rsync="rsync -h --progress"
alias netspeed="curl -s https://raw.githubusercontent.com/sivel/speedtest-cli/master/speedtest.py | python -"
alias docker-ip="sudo docker inspect -f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}'"
alias dockvimgolf='docker run --rm -it -e "key=0da1a0709b69c31106b2c8a7900ce6f5" ghcr.io/filbranden/vimgolf'
alias no-nvidia-screen-tearing='nvidia-settings --assign CurrentMetaMode="nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"'
alias safe-hibernate-dwm="sudo swapoff /dev/nvme2n1p3 && sudo swapon /dev/nvme2n1p3 && systemctl hibernate"
alias gap="git add -p"
alias mpi="mpv --keep-open=yes"
## AWS
alias awsauto="aws --cli-auto-prompt"
## tmux
alias txn="tmux new"
alias txnn="tmux new -s (pwd | sed 's/.*\///g')"
alias txss="tmux new-session -d -s (pwd | sed 's/.*\///g') && tmux switch-client -t (pwd | sed 's/.*\///g')"
alias txa="tmux attach"

# aliases for scripts
alias gas="~/.dotfiles/.scripts/git_auto_sync.sh"
alias wifi="~/.dotfiles/.scripts/wifi.sh"
alias livebook="~/.dotfiles/.scripts/livebook.sh"

export view=nvim
export EDITOR=nvim
export MANPAGER='nvim +Man!'

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

# doom emacs
set -x PATH "$HOME/.dotnet/tools:$PATH"

# calibre
set -Ux CALIBRE_USE_DARK_PALETTE 1

# pyenv
#pyenv init - | source

#### emacs
# vterm
function vterm_printf
    if begin
            [ -n "$TMUX" ]; and string match -q -r "screen|tmux" "$TERM"
        end
        # tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

# lf
function lfcd --wraps="lf" --description="lf - Terminal file manager (changing directory on exit)"
    # `command` is needed in case `lfcd` is aliased to `lf`.
    # Quotes will cause `cd` to not change directory if `lf` prints nothing to stdout due to an error.
    cd "$(command lf -print-last-dir $argv)"
end

# yazi
function y
    set tmp (mktemp -t "yazi-cwd.XXXXXX")
    yazi $argv --cwd-file="$tmp"
    if set cwd (command cat -- "$tmp"); and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
        builtin cd -- "$cwd"
    end
    rm -f -- "$tmp"
end

# vterm clear scrollback
if [ "$INSIDE_EMACS" = vterm ]
    function clear
        vterm_printf "51;Evterm-clear-scrollback"
        tput clear
    end
end

# chmod 755 ~/.dotfiles/.scripts/*
# bash ~/.dotfiles/.scripts/autostart.sh

# tabtab source for packages
# uninstall by removing these lines
[ -f ~/.config/tabtab/fish/__tabtab.fish ]; and . ~/.config/tabtab/fish/__tabtab.fish; or true

# Created by `pipx` on 2023-10-09 15:44:25
set PATH $PATH /home/sumanthyedoti/.local/bin

#### plugin variables
## done
set -U __done_min_cmd_duration 5000
set -U __done_exclude '(^git (?!push|pull|fetch)|lg*|fzf*|lf*|feh*|mpv*|man*)'

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
set -gx PATH $HOME/.cabal/bin /home/sumanthyedoti/.ghcup/bin $PATH # ghcup-env
