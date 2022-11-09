eval "$(starship init zsh)"
# zoxide
eval "$(zoxide init bash)"

# mkdir and cd
mkcd () {
    mkdir -p -- "$1" && cd -P -- "$1"
}

#ALIASES
alias g="git"
alias lg="lazygit"
alias z="zoxide"

# Andorind Studio
export ANDROID_SDK_ROOT=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_SDK_ROOT/emulator
export PATH=$PATH:$ANDROID_SDK_ROOT/platform-tools
