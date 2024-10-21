import datetime

start = datetime.datetime.now()

# The SQLite history backend saves command immediately
# unlike JSON backend that save the commands at the end of the session.
# https://xon.sh/envvars.html#histcontrol
$XONSH_HISTORY_FILE = $XDG_DATA_HOME + "/xonsh/history.sqlite"
$XONSH_HISTORY_BACKEND = "sqlite"
$HISTCONTROL = "erasedups"

$XONSH_SHOW_TRACEBACK = True

$AUTO_PUSHD = True

# Enable mouse support in the prompt_toolkit shell.
# This allows clicking for positioning the cursor or selecting a completion.
# In some terminals however, this disables the ability to scroll back through the history of the terminal.
# To scroll on macOS in iTerm2 press Option key and scroll on touchpad.
$MOUSE_SUPPORT = True

xontrib load \
    vox \
    prompt_starship \
    jedi \
    fish_completer \
    fzf-widgets \
    pipeliner \
    readable-traceback \

# TODO: Why doesn't zoxide work?
    # zoxide

print(f"+{(datetime.datetime.now() - start).total_seconds():.2f} xontrib load")

$fzf_file_binding = "c-t" # Ctrl+T

_gray = "#a0a4b0"
custom_style = {
    "BLUE": "#88c0d0",
    "BOLD_BLUE": "bold #88c0d0",
    "BLACK": "#000000",
    "INTENSE_BLACK": "#000000",
    "GREEN": "#8fbcbb",
    "BOLD_GREEN": "bold #8fbcbb",
    "Token.Comment": _gray,
    "Token.PTK.Aborting": _gray,
    "Token.Comment.Multiline": _gray,
    "Token.Literal.String.Doc": _gray,
    "Token.Comment.PreprocFile": _gray,
    "Token.PTK.AutoSuggestion": _gray,
}

from xonsh.tools import register_custom_style as _register_custom_style
# _register_custom_style("my-nord", custom_style, base="nord")
# $XONSH_COLOR_STYLE="my-nord"
# TODO: use nord theme
# TODO: investigate nested xonsh sessions

import json
aliasFile = p"$HOME/.config/xonsh/aliases.json"
if aliasFile.exists():
    for alias, expansion in json.loads(aliasFile.read_text()).items():
        aliases[alias] = expansion
del json

def _vterm_printf(msg):
    if ${...}.get("TMUX") and ("screen" in $TERM or "tmux" in $TERM):
        print(f"\033Ptmux;\033\033]{msg}\007\033\\")
    elif $TERM.startswith("screen"):
        print(f"\033P\033]{msg}\007\033\\")
    else:
        print(f"\033]{msg}\033\\")

__xonsh__.env["PROMPT_FIELDS"]["vterm_prompt_end"] = lambda: _vterm_printf("51;A" + $(whoami).strip() + "@" + $(hostname).strip() + ":" + $(pwd).strip())
__xonsh__.env["PROMPT"] = "{starship_left}{vterm_prompt_end}"

print(f"+{(datetime.datetime.now() - start).total_seconds():.2f} rc.xsh finished")
