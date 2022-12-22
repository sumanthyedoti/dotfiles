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

# NVM
export NVM_DIR="$HOME/.nvm"
  [ -s "$(brew --prefix nvm)/nvm.sh" ] && \. "$(brew --prefix nvm)/nvm.sh"  # This loads nvm
  [ -s "$(brew --prefix nvm)/etc/bash_completion.d/nvm" ] && \. "$(brew --prefix nvm)/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

# rust
export PATH=$HOME/.cargo/bin:$PATH

# Andorind Studio
export ANDROID_SDK_ROOT=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_SDK_ROOT/emulator
export PATH=$PATH:$ANDROID_SDK_ROOT/platform-tools
