# Blog post with useful stuff; https://thevaluable.dev/zsh-install-configure-mouseless/
# Simple but fast: https://github.com/romkatv/zsh-bench/blob/master/configs/diy%2B%2B/skel/.zshrc
# Zsh benchmarking: https://github.com/romkatv/zsh-bench

#https://unix.stackexchange.com/a/157773/59973
setopt AUTO_PUSHD                  # pushes the old directory onto the stack
setopt PUSHD_MINUS                 # exchange the meanings of '+' and '-'
setopt CDABLE_VARS                 # expand the expression (allows 'cd -2/tmp')
setopt PUSHD_IGNORE_DUPS    # Do not store duplicates in the stack.
setopt PUSHD_SILENT         # Do not print the directory stack after pushd or popd.
#autoload -U compinit && compinit   # load + start completion
zstyle ':completion:*:directory-stack' list-colors '=(#b) #([0-9]#)*( *)==95=38;5;12'


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
if [ -f $XDG_DATA_HOME/spack/share/spack/setup-env.sh ]; then
  source $XDG_DATA_HOME/spack/share/spack/setup-env.sh
fi
