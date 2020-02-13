. ~/.profile

# If not running interactively, do not do anything
[[ $- != *i* ]] && return

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
HIST_STAMPS="yyyy-mm-dd"
plugins=(
    fzf
    git
)
export FZF_BASE=$HOME/.config/zsh/fzf
export ZSH=$HOME/.oh-my-zsh

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# Alias ohmyzsh="mate ~/.oh-my-zsh"

zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' file-sort modification
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' completer _complete _files

zstyle :compinstall filename '~/.zshrc'
autoload -Uz compinit
compinit

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e

# # time commands
# time_cmd_file="$HOME/.local/cmd_time/${$}.time"
# time_cmd_dir="$(dirname ${time_cmd_file}\")"
# if [ ! -d "${time_cmd_dir}" ]
# then
# 	mkdir -p "${time_cmd_dir}"
# fi

# preexec() {
# 	echo "${2}" > "${time_cmd_file}"
# 	date +%s >>  "${time_cmd_file}"
# }

# precmd () {
# 	if [ -e "${time_cmd_file}" ]
# 	then
# 		cmd=$(head -n +1 "${time_cmd_file}")
# 		start=$(tail -n 1 "${time_cmd_file}")
# 		rm "${time_cmd_file}"
# 		end=$(date +%s)
# 		elapsed=$(expr "${end}" - "${start}")
# 		if [ "${elapsed}" -gt 240 ]
# 		then
# 			echo -n '\a'
# 			ntfy -t "Command complete" send "${cmd}"
# 		fi
# 	fi
# }

disable r

# aliases
alias l='exa -alrs modified'

# start tmux
[[ $TERM != "dumb" ]] && [[ $TERM != "screen" ]] && [[ -z "$TMUX" ]] && exec tmux
