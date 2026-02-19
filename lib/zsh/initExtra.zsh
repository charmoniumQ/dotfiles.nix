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

# Rust
if [[ -f $XDG_DATA_HOME/cargo/env ]]; then
  . $XDG_DATA_HOME/cargo/env
fi

vterm_printf() {
    if [ -n "$TMUX" ] \
        && { [ "${TERM%%-*}" = "tmux" ] \
            || [ "${TERM%%-*}" = "screen" ]; }; then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

HISTSIZE=100000
SAVEHIST=100000
