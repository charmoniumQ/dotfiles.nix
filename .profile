PATH="$HOME/bin:$PATH"

if which emc > /dev/null
then
	EDITOR=emc
else
	EDITOR=nano
fi
