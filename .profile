PATH="$HOME/.local/mbin:$HOME/.local/bin:$PATH"

alias l='ls -ahlt'

if which emc > /dev/null
then
	EDITOR=emc
	#PAGER=emc-pager
else
	EDITOR=nano
fi
