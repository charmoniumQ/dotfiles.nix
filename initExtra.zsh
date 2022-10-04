# This conflicts with the r script.
if which r &> /dev/null; then
  disable r
fi

# Emacs/vterm integration
# https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n $EMACS_VTERM_PATH ]] \
    && [[ -f $EMACS_VTERM_PATH/etc/emacs-vterm-zsh.sh ]]; then
      source $EMACS_VTERM_PATH/etc/emacs-vterm-zsh.sh
fi

# Spack
if [ -d $XDG_DATA_HOME/spack ]; then
  SPACK_USER_CONFIG_PATH=$XDG_DATA_HOME/spack-data
  SPACK_USER_CACHE_PATH=$XDG_CACHE_HOME/spack
  source $XDG_DATA_HOME/spack/share/spack/setup-env.sh
fi

# Spaceship theme
if [ -f ~/.config/zsh/plugins/spaceship-prompt/spaceship.zsh-theme ]; then
	. ~/.config/zsh/plugins/spaceship-prompt/spaceship.zsh-theme
fi
