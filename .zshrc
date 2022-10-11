eval "$(starship init zsh)"
s
# mkdir and cd
mkcd () {
    mkdir -p -- "$1" && cd -P -- "$1"
}
