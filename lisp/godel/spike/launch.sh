#!/usr/bin/env bash
# Launch a hooked notdiamond-claude session in a spike tmux window.
# Usage: launch.sh NAME DIR [extra claude args...]
name=$1 dir=$2; shift 2
sid=$(python3 -I -c 'import uuid;print(uuid.uuid4())')
here=$(cd "$(dirname "$0")" && pwd)
hooks=$(python3 -I - "$here/godel-hook" <<'PY'
import json, sys
events = ["SessionStart", "UserPromptSubmit", "PreToolUse", "PostToolUse",
          "Notification", "Stop", "SessionEnd", "PreModelSwitch"]
print(json.dumps({"hooks": {e: [{"hooks": [{"type": "command", "command": f"{sys.argv[1]} {e}"}]}] for e in events}}))
PY
)
tmux -L godel-spike -f /dev/null new-session -d -s "$name" -x 160 -y 45 -c "$dir" \
  env -i HOME="$HOME" USER="$USER" LANG="${LANG:-en_US.UTF-8}" TERM=xterm-256color PATH="$PATH" \
  CLAUDE_CONFIG_DIR="$CLAUDE_CONFIG_DIR" GODEL_SERVER=godel-spike \
  notdiamond-claude --session-id "$sid" --settings "$hooks" "$@"
echo "$sid"
