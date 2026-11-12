# godel Phase 0 spike: findings

Run 2026-11-11 against notdiamond 0.2.16-rc.1 and Claude Code 2.1.308 → 2.1.322
(it auto-updated during the spike), Doom Emacs on Emacs 30.2 in terminal
mode. Scripts are in `spike/`. Everything ran in scratch directories, a
separate tmux server, and separate Emacs servers. The scratch files,
processes and test transcripts were removed afterwards, and
`~/.claude-work/settings.json` was checked byte-for-byte against a snapshot.

## Summary

| Check | Result |
|---|---|
| Wrapper passes `--session-id`, `--effort`, `--permission-mode`, `--settings` | ✅ |
| Wrapper strips `--model` and `model` in `--settings` | ✅ as expected; sessions start on `notdiamond[1m]` |
| godel hooks merged with the wrapper's | ✅ per event, both run |
| Hook → `godel-hook` → `emacsclient` → Emacs | ✅ |
| Model list from the wrapper's `--settings` | ❌ not in 0.2.16 (see 1) |
| Switch model without changing the user's default | ✅ only via the picker + `s` (see 2) |
| Confirm the active model | ✅ `PreModelSwitch` hook; transcript also shows it |
| Prompt sent after the switch runs on the new model | ✅ |
| Multi-line prompts through `claude-code-ide-send-prompt` | ✅ arrive as one prompt |
| `claude-code-ide` with per-launch flags, one session per worktree | ✅ global value unchanged afterwards |
| Terminal load | ✅ plenty of headroom (see 5) |
| `:wq` / `:q!` in compose buffers | ✅ via keymap remaps, like with-editor (see 6) |
| Workspace trust dialog | ⚠ blocks new repos; worktrees inherit trust (see 3) |

## 1. The model list

The `modelPicker` found in the wrapper code belongs to its internal `claude2`
command. `notdiamond claude` instead adds one custom option through env vars
(`ANTHROPIC_CUSTOM_MODEL_OPTION=notdiamond[1m]`, name "Not Diamond") and
passes `--model notdiamond[1m]`. The rest of the `/model` menu is Claude
Code's built-in list, which lives in the Claude binary. The temp-config probe
works (1.2s, no session) but can't see that list.

The menu as shown on 2026-11-11:

```
1. Default (recommended)  Opus 5.5 · Best for everyday, complex tasks
2. Opus                   Opus 5.5 · Best for everyday, complex tasks
3. Fable                  Fable 5.1 · Most capable for your hardest and longest-running tasks
4. Sonnet                 Sonnet 5.5 · Efficient for routine tasks
5. Haiku                  Haiku 5.5 · Fastest for quick answers
6. Not Diamond            Routes each turn to the best model and effort
◐ Medium effort ←/→ to adjust
```

The only live source is the menu itself. Reading it works (from tmux and from
the ghostel buffer), but:
- In a short window the menu scrolls (`↑ 5. Haiku`), so reading the **full**
  list needs a tall terminal of its own.
- A session that opens `/model` and exits leaves a ~2 KB transcript, while a
  session with no input leaves none. Run the reader in a scratch directory
  and delete its transcript (godel knows the session ID).

## 2. Switching model and effort

- `/model opus` **saves the model as the user's default**
  (`"model": "opus"` in `~/.claude-work/settings.json`).
- `/effort high` **saves too** (`modelSettings.<model>.effortLevel`).
- The picker footer says: `Enter to set as default · s to use this session
  only`. Choosing a row and pressing `s` gives "Set model to Sonnet 5.5 for
  this session only"; settings are unchanged.
- ←/→ in the picker sets effort, and `s` applies both for this session only:
  "Set model to Sonnet 5.5 for this session only with xhigh effort".
- **The picker wraps around**: Up from row 1 goes to the last row. Moving to a
  row means reading the `❯ N.` cursor row and sending the exact difference.
- `--effort` at launch takes effect ("with low effort") and is not saved.
- `PreModelSwitch` hook payload: `from_model`, `to_model`, `requested_model`,
  `source` (`picker`). This confirms the switch.
- The transcript of a turn run after switching to Sonnet showed
  `claude-sonnet-5-5[1m]`.

## 3. Startup and trust

- In a directory Claude hasn't seen, startup stops on "Is this a project you
  created or one you trust?". **The default option is "No, exit"** and no hook
  fires until it's answered.
- A **worktree of a trusted repo is trusted automatically**; there's no dialog.
- After trust: `SessionStart` arrives in 0.47s, and the input box is visible
  at 0.58s. Wait for the input box, not just the hook.
- `SessionStart` payload: `session_id` (godel's), `cwd`, `transcript_path`,
  `source`, `model`.
- Claude Code's default permission mode is now **auto** (shown at startup).

## 4. Hook payloads relevant to the board

| Event | Useful fields |
|---|---|
| `UserPromptSubmit` | `prompt`, `permission_mode` |
| `PreToolUse` | `tool_name`, `tool_input` (e.g. `Bash`, `{"command": "touch …"}`) |
| `Notification` | `notification_type: "permission_prompt"`, `message: "Claude needs your permission"`. Fired right when the dialog appeared |
| `Stop` | `last_assistant_message`, `effort.level` |
| `SessionEnd` | `reason` (e.g. `prompt_input_exit`) |
| `PreModelSwitch` | see 2 |

If Emacs isn't running, `emacsclient` fails and the event is lost (the hook
script exits 0 so Claude is not affected). godel must rebuild state on
startup, for example from the transcripts.

## 5. Terminal load

Real Claude output: **0 bytes while idle**, about **2 KB/s while working**
(10 KB over a 5-second turn).

Load test: 8 ghostel buffers each redrawing a 12-line block 20×/s (~26 KB/s,
about 13× real output), Doom in terminal mode:

| Setup | Emacs CPU | `emacsclient` round trip | 50ms timer lateness |
|---|---|---|---|
| 2 idle Claude sessions | 1% | 2ms (max 3) | max 1ms |
| + 8 load terminals, hidden | 10% | 2ms (max 3) | max 1ms |
| + 2 of them visible | 41% | 3ms (max 28) | avg 3.8ms, max 101ms |

Hidden sessions cost little; visible, fast-redrawing terminals cost the
most. At real output rates there's plenty of headroom, so the tmux backend
isn't needed for performance. GUI Emacs wasn't measured.

## 6. Compose buffers

with-editor (magit's commit buffers) doesn't use file semantics; it remaps
evil commands in its keymap. The same remaps in a test minor mode, run
through real ex commands in Doom:

| Input | Result |
|---|---|
| `:wq`, `:x`, `ZZ` | finish (`evil-save-and-close`, `evil-save-modified-and-close`) |
| `:q`, `:q!`, `ZQ` | cancel (`evil-quit`) |

Emacs doesn't quit and windows aren't closed by evil; the remapped command
decides. As in magit, **`:q` always cancels**, even after `:w`.

## 7. Other notes

- `claude-code-ide` buffer names come from the directory's base name
  (`*claude-code[a]*`), so same-named worktrees in two repos collide. Set
  `claude-code-ide-buffer-name-function`.
- `claude-code-ide-send-prompt` blocks Emacs for 0.1s (`sit-for`) per prompt.
- `notdiamond doctor`'s `CLAUDE_CONFIG_DIR` warning only affects the wrapper's
  telemetry ("telemetry for this session will be incomplete"), not hooks or
  sessions.
- There was no Emacs server running before the spike; `godel-mode` must start
  one.

## 8. Follow-up, 2026-11-12: model list and model pin

These replace the screen-reading approach from sections 1 and 2.

**Model list via `initialize`.** Sending the Agent SDK's `initialize` control
request to `notdiamond-claude -p --input-format stream-json --output-format
stream-json --verbose` returns `models`: the same six entries as the `/model`
menu (incl. Not Diamond), each with `value`, `displayName`, `description`,
`supportedEffortLevels` and `resolvedModel`. 3.3s, no model call, no
transcript, settings unchanged. The wrapper prints two plain-text lines to
stdout before the JSON.

**Model pin.** `NOTDIAMOND_RESEARCH_MODEL_PIN`, `NOTDIAMOND_RESEARCH_EFFORT_PIN`
and `NOTDIAMOND_ROUTING=off` in the session's environment make the wrapper
pass `--model <pin>` and set `effortLevel` (`spike/pin-test.sh`):

| Pin | Banner | `SessionStart` model | Transcript | Proxy log |
|---|---|---|---|---|
| `sonnet` / high | Sonnet 5.5 with high effort | `claude-sonnet-5-5` | `claude-sonnet-5-5[1m]` | `passthrough` |
| `claude-fable-5-1[1m]` / medium | Fable 5.1 with medium effort | `claude-fable-5-1` | `claude-fable-5-1` | `passthrough` |

`~/.claude-work/settings.json` unchanged. The first request of each session
got a 429 (account rate limit, also seen on router sessions) and Claude's
retry succeeded.

**Routing-off leak (from code, not tested live).** When no proxy is running,
the wrapper starts one with `env: process.env`, and the proxy checks
`process.env.NOTDIAMOND_ROUTING === "off"`. A proxy started by a pinned launch
would therefore route nothing for any session. Not tested live because this
conversation runs through the same proxy. The running proxy (pid 3681887) was
checked through `/proc/<pid>/environ`: no pin or routing variables. godel must
start the proxy itself, with a clean environment, before pinned launches
(`notdiamond status --json` / `notdiamond start`).

**Not Diamond alongside a pinned session (2026-11-12).** `spike/pin-test.sh`
(model `-` = no pin) ran a Not Diamond session while a pinned Sonnet session
was still running. Proxy log during the test:

| Time | Model | Effort | Source | Decision |
|---|---|---|---|---|
| 15:24:08 | claude-sonnet-5-5 | - | passthrough | — (pinned session) |
| 15:24:14 | claude-opus-5-5 | xhigh | server | simple (429, then retried) |
| 15:24:37 | claude-opus-5-5 | xhigh | reuse | — |
| 15:24:38 | claude-haiku-5-5 | low | chat | trivial |

The Not Diamond session was routed normally. Routing off applies only to the
pinned session's own launch; the shared proxy's environment (checked through
`/proc`) has no pin or routing variables.
