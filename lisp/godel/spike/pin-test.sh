#!/usr/bin/env bash
# Start a pinned interactive session in tmux, send a tiny prompt, report the
# model from hooks and transcript. Needs the godel-spike Emacs server running.
# Usage: pin-test.sh NAME DIR MODEL EFFORT
# MODEL "-" = no pin (Not Diamond router). KEEP=1 leaves the session running.
name=$1 dir=$2 model=$3 effort=$4
here=$(cd "$(dirname "$0")" && pwd)
sl() { python3 -I -c "import time;time.sleep($1)"; }
E() { emacsclient -s godel-spike --eval "$1"; }
sid=$(python3 -I -c 'import uuid;print(uuid.uuid4())')
hooks=$(python3 -I - "$here/godel-hook" <<'PY'
import json, sys
events = ["SessionStart", "UserPromptSubmit", "Stop", "SessionEnd", "PreModelSwitch"]
print(json.dumps({"hooks": {e: [{"hooks": [{"type": "command", "command": f"{sys.argv[1]} {e}"}]}] for e in events}}))
PY
)
E '(setq godel-spike-events nil)' >/dev/null
if [ "$model" = "-" ]; then pin=(); effort_flag=()
else pin=(NOTDIAMOND_RESEARCH_MODEL_PIN="$model" NOTDIAMOND_RESEARCH_EFFORT_PIN="$effort" NOTDIAMOND_ROUTING=off); effort_flag=(--effort "$effort"); fi
tmux -L godel-spike -f /dev/null new-session -d -s "$name" -x 160 -y 45 -c "$dir" \
  env -i HOME="$HOME" USER="$USER" LANG="${LANG:-en_US.UTF-8}" TERM=xterm-256color PATH="$PATH" \
  CLAUDE_CONFIG_DIR="$CLAUDE_CONFIG_DIR" GODEL_SERVER=godel-spike \
  "${pin[@]}" notdiamond-claude --session-id "$sid" "${effort_flag[@]}" --settings "$hooks"
cap() { tmux -L godel-spike capture-pane -p -t "$name"; }
for i in $(seq 1 60); do
  if cap | grep -q 'Yes, I trust this folder'; then tmux -L godel-spike send-keys -t "$name" Down Enter; fi
  cap | grep -q '❯ Try' && break; sl 0.25
done
echo "== banner"; cap | grep -E 'Claude Code v|effort ·|with .* effort' | head -3
tmux -L godel-spike send-keys -t "$name" -l 'Reply with just the word OK.'; sl 0.3
tmux -L godel-spike send-keys -t "$name" Enter
for i in $(seq 1 120); do E '(cadr (car godel-spike-events))' | grep -q Stop && break; sl 0.5; done
echo "== hooks"
E '(godel-spike-dump)' | python3 -I -c 'import json,sys;d=json.loads(json.loads(sys.stdin.read()));[print(" ", e["event"], {k:e["data"][k] for k in ("model","from_model","to_model","effort","last_assistant_message") if k in e["data"]}) for e in d]'
tp=$(ls "$CLAUDE_CONFIG_DIR"/projects/*/"$sid".jsonl 2>/dev/null | head -1)
echo "== transcript models"; jq -r 'select(.type=="assistant") | .message.model' "$tp" | sort | uniq -c
if [ -z "$KEEP" ]; then tmux -L godel-spike send-keys -t "$name" -l '/exit'; sl 0.3; tmux -L godel-spike send-keys -t "$name" Enter; sl 2; fi
echo "sid=$sid"
