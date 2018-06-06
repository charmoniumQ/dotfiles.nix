. ~/.profile
ZSH_THEME="robbyrussell"
CASE_SENSITIVE="false"
HYPHEN_INSENSITIVE="true"
DISABLE_AUTO_UPDATE="false"
export UPDATE_ZSH_DAYS=13
DISABLE_LS_COLORS="false"
DISABLE_AUTO_TITLE="false"
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"
HIST_STAMPS="yyyy-mm-dd"
plugins=(
  git
)
export ZSH=$HOME/.oh-my-zsh
source $ZSH/oh-my-zsh.sh

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

time_cmd_file="$HOME/.local/cmd_time/${$}.time"
time_cmd_dir="$(dirname ${time_cmd_file}\")"
if [ ! -d "${time_cmd_dir}" ]
then
	echo hi
	mkdir -p "${time_cmd_dir}"
fi

preexec() {
	echo "${2}" > "${time_cmd_file}"
	date +%s >>  "${time_cmd_file}"
}

precmd () {
	if [ -e "${time_cmd_file}" ]
	then
		cmd=$(head -n +1 "${time_cmd_file}")
		start=$(tail -n 1 "${time_cmd_file}")
		end=$(date +%s)
		elapsed=$(expr "${end}" - "${start}")
		if [ "${elapsed}" -gt 40 ]
		then
			echo -n '\a'
			notify-send "Command complete" "${cmd}"
		fi
	fi
}

# If not running interactively, do not do anything
[[ $- != *i* ]] && return
[[ -z "$TMUX" ]] && exec tmux
