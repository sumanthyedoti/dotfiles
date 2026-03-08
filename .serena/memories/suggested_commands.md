# Suggested commands
- `stow .` to symlink the dotfiles into `$HOME` per the README.
- `tmux -f ~/.tmux.conf start-server` or `tmux source-file ~/.tmux.conf` to validate/reload tmux config.
- `git status --short` and `git diff -- <path>` to inspect pending dotfile changes.
- `rg <pattern> .` and `rg --files .` for fast search in the repo.
- `nvim ~/.tmux.conf` or other dotfiles for editing.
- `tmux` plugin manager install/update is documented as `prefix + I` in the README.