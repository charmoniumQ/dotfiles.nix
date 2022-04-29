# Short-term

- [ ] Make vterm work in Emacs
- [x] Use Keepassxc to store SSH-keys and GPG-keys
- [ ] emacsclient/server launcher script
- [ ] Write launchers for apps installed by Nix (Emacs)
- [x] Make .local/bin scripts
  - [x] Add to $PATH
  - [x] Install deps
- [ ] Make an rclonesync-v2 script
- [ ] Split into modules
- [x] Install scc
- [ ] Switch oh-my-zsh to prezto + zplug
- [ ] Install spaceship theme

# Long-term

- [x] Switch .config/emacs/init.el package manager from el-get to ~straight.el~ Nixpkgs.
- [ ] ackage rclonesync-v2
- [ ] Nix install fira-code
- [ ] Don't set TERM=xterm-256color
  - While this makes everything work, the author of kitty recommends against it, because it locks me in to only those features that xterm supports.
  - I think the solution is to add kitty to the termcap database.
  - See: https://github.com/kovidgoyal/kitty/issues/713
- [ ] Add a step to bootstrap-post.sh that configures the connection.

https://www.reddit.com/r/emacs/comments/8hbp8e/what_window_manager_and_terminal_emulator_do_you/
