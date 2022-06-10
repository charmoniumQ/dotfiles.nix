# This conflicts with the r script.
disable r

# This gives us a hook to prepend to PS1 especiallhttps://github.com/direnv/direnv/wiki/PS1 in direnv.
# https://github.com/direnv/direnv/wiki/PS1
export PS1="\$PREPEND_TO_PS1$PS1"

export KEEPASSDB=${HOME}/box/Database.kdbx

# emacs-vterm integration
# https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
fi

# For Spack
#source ${HOME}/.local/opt/spack/share/spack/setup-env.sh

# # >>> conda initialize >>>
# # !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/home/sam/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/home/sam/miniconda3/etc/profile.d/conda.sh" ]; then
#         . "/home/sam/miniconda3/etc/profile.d/conda.sh"
#     else
#         export PATH="/home/sam/miniconda3/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# # <<< conda initialize <<<
