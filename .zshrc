eval "$(starship init zsh)"
s
# mkdir and cd
mkcd () {
    mkdir -p -- "$1" && cd -P -- "$1"
}

#ALIASES
alias lg="lazygit"

# Andorind Studio
export ANDROID_SDK_ROOT=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_SDK_ROOT/emulator
export PATH=$PATH:$ANDROID_SDK_ROOT/platform-tools
