if status is-interactive
    # Commands to run in interactive sessions can go here
end

# mkdir and cd
function mkcd
    mkdir -p -- $argv && cd -- $argv
end

# Mac config
if test (uname) = "Darwin"
  # Set PATH, MANPATH, etc., for Homebrew.
  eval (/opt/homebrew/bin/brew shellenv)
end

# starship
starship init fish | source
