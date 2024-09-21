## Setup

- Install the Tools and Dependecies first
- git clone this repo into `~/.dotfiles`
- run `stow .`R
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
  - jupyter
- fish
  - [fisher](https://github.com/jorgebucaran/fisher)
    - [nvm](https://github.com/jorgebucaran/nvm.fish)
    - [fzf](https://github.com/PatrickF1/fzf.fish)
    - [docker](https://github.com/halostatue/fish-docker)
    - [tide](https://github.com/IlanCosman/tide)
      - `tide configure`
    - [sponge](https://github.com/meaningful-ooo/sponge)
    - [gitnow](https://github.com/joseluisq/gitnow)
    - [done](https://github.com/franciscolourenco/done)
    - [autopair](https://github.com/jorgebucaran/autopair.fish)
    - [replay](https://github.com/jorgebucaran/replay.fish)
  - [oh-my-fish](https://github.com/oh-my-fish/oh-my-fish)
    - [pyenv](https://github.com/oh-my-fish/plugin-pyenv)
- kitty
- alacritty
- nvim
  - [lua-language-server](https://github.com/sumneko/lua-language-server/wiki/Getting-Started)
  - LSPs
    - stylua,
    - typescript, typescript-language-server,
    - @prisma/language-server,
    - vscode-html-languageserver, vscode-css-languageserver
    - golangci-lint
    - rust-analyzer
    - pyright
    - elixir-ls
    - [clojure-lsp](https://clojure-lsp.io/installation/)
  - Language tools
    - JS/TS
      - eslint_d
  - DAPs
    - debugpy
- tmux
  - [tpm](https://github.com/tmux-plugins/tpm)
  - [tmux2html] (https://github.com/tweekmonster/tmux2html)
  - [powerline-fonts] (https://github.com/powerline/fonts)
  - `prefix + r` - reload
  - `prefix + I` to fetch the plugins
- emacs
  - `M-x all-the-icons-install-fonts`
    - install `libtool-bin`
  - install `coreutils` (in mac, for dired)
  - install `mermaid-cli `(@mermaid-js/mermaid-cli)
- cmake
- docker
  - docker-buildx

### CLI tools

- starship
- ripgrep
- zoxide
- bat
- exa
- fd
- proc
- xh
- ncdu
- duf
- jq
- ranger
- lf
  - lf-gadgets
- trash-cli
- lazygit
  - link macos config file manually (https://github.com/jesseduffield/lazygit/issues/1341)
- csvlens
- glow - markdown previewer
- pandoc
- git-delta
- [roswell](https://github.com/roswell/roswell)
- [hyperfine](https://github.com/sharkdp/hyperfine)
- [fzf](https://github.com/junegunn/fzf)
- tree-sitter `npm install -g tree-sitter-cli`
- rsync
- password store (pass)
  - gpg
  - pinentry
- tldr
- github-cli (https://cli.github.com/manual/gh)
- `npm i -g @343dev/optimizt`

#### Dev Tools

- postgresql

#### Linux

- xrandr
- [XMonad WM](https://xmonad.org/download.html)
  - xmonad-contrib (community library for additional features)
  - xmobar
    - after change, `killall xmobar` and reload(`M-q`) XMonad
  - dmenu
  - xterm
  - nitrogen
  - picom
- dunst
- scrot
- mpv
- yt-dlp
- feh
- ffmpeg
- pamixer
- cronie
- sxhkd
- xdotool
- tesseract, tesseract-data-eng
- copyq
