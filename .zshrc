eval "$(starship init zsh)"
# zoxide
eval "$(zoxide init zsh)"

# mkdir and cd
mkcd () {
    mkdir -p -- "$1" && cd -P -- "$1"
}

#ALIASES
alias .="pwd"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

alias clr="clear"

alias ls="ls --color=auto"
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
alias fzfp='fzf --preview "bat --style numbers,changes --color=always {} | head -200"' ## with preview

alias lf="lfu ."

alias trm="trash-put"
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
alias dockvimgolf='docker run --rm -it -e "key=0da1a0709b69c31106b2c8a7900ce6f5" ghcr.io/filbranden/vimgolf'
alias no-nvidia-screen-tearing='nvidia-settings --assign CurrentMetaMode="nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"'
alias safe-hibernate-dwm="sudo swapoff /dev/nvme2n1p3 && sudo swapon /dev/nvme2n1p3 && systemctl hibernate"
alias gap="git add -p"
alias mpi="mpv --keep-open=yes"
alias uuid="uuidgen | tr '[:upper:]' '[:lower:]'"
## AWS
alias awsauto="aws --cli-auto-prompt"
## tmux
alias txn="tmux new"
alias txnn="tmux new -s (pwd | sed 's/.*\///g')"
alias txss="tmux new-session -d -s (pwd | sed 's/.*\///g') && tmux switch-client -t (pwd | sed 's/.*\///g')"
alias txa="tmux attach"

# aliases for scripts
alias gas="~/.dotfiles/.scripts/git_auto_sync.sh"

export view=nvim
export EDITOR=nvim

# NVM
if [[ $(uname) == "Darwin" ]]; then
export NVM_DIR="$HOME/.nvm"
  [ -s "$(brew --prefix nvm)/nvm.sh" ] && \. "$(brew --prefix nvm)/nvm.sh"  # This loads nvm
  [ -s "$(brew --prefix nvm)/etc/bash_completion.d/nvm" ] && \. "$(brew --prefix nvm)/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
fi

# rust
export PATH=$HOME/.cargo/bin:$PATH

# Andorind Studio
export ANDROID_SDK_ROOT=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_SDK_ROOT/emulator
export PATH=$PATH:$ANDROID_SDK_ROOT/platform-tools

#### emacs
export PATH="$HOME/.config/emacs/bin:$PATH"

# vterm
vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
# vterm clear scrollback `C-C c_l`
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# chmod 755 ~/.dotfiles/.scripts/*
# bash ~/.dotfiles/.scripts/autostart.sh

. "$HOME/.local/bin/env"
