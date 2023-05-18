eval "$(starship init bash)"
# zoxide
eval "$(zoxide init bash)"

# mkdir and cd
mkcd () {
    mkdir -p -- "$1" && cd -P -- "$1"
}

# ALIASES
alias g="git"
alias lg="lazygit"
alias z="zoxide"
alias emacs="emacs -mm"

# aliases for scripts
alias gas="~/.dotfiles/.scripts/git_auto_sync.sh"

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
export PATH=$HOME/.emacs.d/bin:$PATH

#### emacs
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
    function clear() {
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# chmod 755 ~/.dotfiles/.scripts/*
# bash ~/.dotfiles/.scripts/autostart.sh
