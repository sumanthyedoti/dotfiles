# Style and completion notes
- Prefer minimal, native configuration syntax for each tool rather than abstraction layers.
- Preserve existing keybinding conventions and comments when editing dotfiles.
- Before claiming a config fix is complete, run the tool's own parser or reload command when possible (for tmux, validate with `tmux -f ~/.tmux.conf start-server` or source-file on a separate socket).
- Be careful with quoting in config languages; parser errors may surface later than the actual bad line.