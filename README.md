## Setup

- Install the Tools and Dependecies first
- git clone this repo into `~/.dotfiles`
- run `stow .`
- Install Fonts
  - Hack (nerd)
  - Jetbrains Mono (nerd)
  - Roboto Slab
  - Roboto Mono
  - [Iosevka](https://github.com/be5invis/Iosevka)
- nvim
  - [Prerequisites](https://github.com/neovim/neovim/wiki/Building-Neovim#build-prerequisites)
  - `:Lazy Sync`
  - `:UpdateRemotePlugins`
  - `:call firenvim#install(0)`

## Dependecies

- stow
- brew (Mac)
  - rlwrap
- rbenv (Mac)
- node
  - nvm
- elixir
- haskell
  - ghcup
    - stack
    - ghc
- rust
  - rustup
- go
- clojure
  - Leiningen
- common-lisp - [get-started](https://lisp-lang.org/learn/getting-started/) (use roswell instead)
  - quicklisp
  - sbcl
- python
  - pip
  - pyenv (?)
- fish
  - [fisher](https://github.com/jorgebucaran/fisher)
    - [nvm](https://github.com/jorgebucaran/nvm.fish)
    - [fzf](https://github.com/PatrickF1/fzf.fish)
  - [oh-my-fish](https://github.com/oh-my-fish/oh-my-fish)
    - [pyenv](https://github.com/oh-my-fish/plugin-pyenv)
- kitty
- alacritty
- nvim
  - [lua-language-server](https://github.com/sumneko/lua-language-server/wiki/Getting-Started)
  - install LSPs
    - stylua,
    - typescript, typescript-language-server
    - vscode-languageserver
    - golangci-lint
    - rust-analyzer
    - pyright
    - elixir-ls
    - [clojure-lsp](https://clojure-lsp.io/installation/)
  - install DAPs
    - debugpy
- tmux
  - [tpm](https://github.com/tmux-plugins/tpm)
  - `prefix + I` to fetch the plugins
- emacs
  - `M-x all-the-icons-install-fonts`
    - install `libtool-bin`
  - install `coreutils` (in mac, for dired)
- cmake
- [XMonad WM](https://xmonad.org/download.html)
  - xmonad-contrib (community library for additional features)
  - xmobar
    - after change, `killall xmobar` and reload(`M-q`) XMonad
  - dmenu
  - xterm
  - nitrogen
  - picom

### Linux WM

- sxhkd
- mpv
  - youtube-dl
- ffmpeg

### CLI tools

- starship
- ripgrep
- zoxide
- bat
- exa
- fd
- lazygit
  - link macos config file manually (https://github.com/jesseduffield/lazygit/issues/1341)
- glow - markdown previewer
- pandoc
- [roswell](https://github.com/roswell/roswell)
- [hyperfine](https://github.com/sharkdp/hyperfine)
- [ripgrep](https://github.com/BurntSushi/ripgrep#installation)
- [fzf](https://github.com/junegunn/fzf)
- tree-sitter `npm install -g tree-sitter-cli`
- rsync
- password store (pass)
  - gpg
  - pinentry
- tldr
- cht.sh
