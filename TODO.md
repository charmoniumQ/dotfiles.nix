# Short-term

- Use Keepassxc to store SSH-keys and GPG-keys
- emacsclient launcher script
- Make .local/bin scripts
  - Add to $PATH
  - Install deps
- Make an rclonesync-v2 script
- Split into modules
- Make copy/paste work in tmux
- Fix startup
  - Start tmux in zshrc
  - Start zsh (default-shell) in tmux
- Install scc
- Switch oh-my-zsh to prezto + zplug
- Install spaceship theme

# Long-term

- Switch .config/emacs/init.el package manager from el-get to straight.el
- Package rclonesync-v2
- Nix install fira-code
- Don't set TERM=xterm-256color
  - While this makes everything work, the author of kitty recommends against it, because it locks me in to only those features that xterm supports.
  - I think the solution is to add kitty to the termcap database.
  - See: https://github.com/kovidgoyal/kitty/issues/713
- Add a step to bootstrap-post.sh that configures the connection.
