eval "$(starship init bash)"

# mkdir and cd
mkcd () {
    mkdir -p -- "$1" && cd -P -- "$1"
}

# ALIASES
alias lg="lazygit"
