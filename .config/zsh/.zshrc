# This conflicts with the r script.
disable r

# This gives us a hook to prepend to PS1 especiallhttps://github.com/direnv/direnv/wiki/PS1 in direnv.
# https://github.com/direnv/direnv/wiki/PS1
export PS1="\$PREPEND_TO_PS1$PS1"
