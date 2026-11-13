# godel: an agent development environment for Emacs

Status: Phase 0 (spike) done 2026-11-11, findings in `SPIKE.md`. Phase 1
(MVP) built 2026-11-12; see "Phase 1: what was built" under Phases. Written
2026-11-10.

## What it is

An agent development environment (ADE) helps you manage agents that write
code, where an IDE helps you write code yourself. The term and the workflow
come from Paradev. In an ADE you work with **tasks**: each task has an agent,
an isolated environment, a branch and a result. godel brings that loop into
Doom Emacs:

> create task -> isolated worktree -> launch agent -> watch status ->
> review diff -> send feedback -> ship -> clean up

godel mostly connects pieces Emacs already has: git worktrees through magit,
Doom workspaces, `claude-code-ide` on ghostel, `magit-section`, and
`transient`. The new parts are the task model, status tracking driven by
hooks, the board, and the review loop.

## Decisions

| Topic | Decision |
|---|---|
| Agent | Claude only, behind a small backend interface |
| Session layer | Build on `claude-code-ide`; if it blocks us, switch to plain ghostel + `claude` |
| CLI | `notdiamond-claude`. Do not use the `claude` on PATH, which is Paradev's wrapper |
| Claude binary | `claudeBinaryPath` in `~/.notdiamond/config.json` points at `~/.local/bin/claude`, so sessions don't go through Paradev's wrapper (set 2026-11-11) |
| Model choice | Chosen per task from the list Claude reports through the SDK `initialize` request. Applied at launch with the wrapper's model pin (env vars), never with `/model X` or `/effort X`, which save the user's default. The pin is checked when the wrapper updates (`godel-models-check-pin`); there is no TUI fallback |
| Paradev | Fully separate: do not read `.paradev/` config or import Paradev worktrees |
| Name / prefix | `godel` / `godel-`, code in `lisp/godel/` |
| Comment key in diffs | `C`, only inside godel task diffs |
| Leader prefix | `SPC G` (`SPC g` stays git) |

## Core rules

1. **One task = one worktree = one branch = one author session.** Only one
   agent writes to a given worktree. This is what makes running agents in
   parallel safe.
2. **A workspace only displays a task.** godel creates a Doom workspace the
   first time you open a task, and you can close it at any time. The task and
   its agent keep running without it.
3. **Hooks report status; godel never reads the terminal to work it out.**
   The board can then show every task's state without any terminal attached.
4. **Nothing slow runs synchronously.** git, setup scripts and headless
   reviews all go through `make-process` with callbacks.
5. **Everything has an API.** Every action is a `godel-api-*` function that
   takes a task ID. Every state change goes through
   `godel-event-functions`. That keeps the board, notifications and future
   frontends (Slack, web, phone) pluggable.

## Architecture

```
lisp/godel/
  godel.el           ; godel-mode (global minor mode), SPC G keymap, defcustoms
  godel-task.el      ; task model and store
  godel-worktree.el  ; worktree/branch lifecycle, setup/teardown scripts
  godel-agent.el     ; backend protocol + claude-code-ide backend
  godel-tui.el       ; reading the Claude TUI: input ready, trust dialog
  godel-models.el    ; model/effort list via the SDK initialize request, cache, pin, proxy guard
  godel-status.el    ; hook event receiver -> state machine -> godel-event-functions
  godel-board.el     ; godel-board-mode (magit-section): full board and sidebar views
  godel-sidebar.el   ; the sidebar window: SPC G o, auto-open in task workspaces
  godel-compose.el   ; godel-compose-mode (prompt / comment / feedback editing)
  godel-new.el       ; new-task transient (model/effort pickers) and task creation
  godel-review.el    ; task diffs, comments, review list, feedback, ask the author
  godel-reviewer.el  ; independent reviewer: structured review and walkthrough
  godel-ship.el      ; author-drafted commit message and PR, push, merged state
  godel-api.el       ; godel-api-* entry points for external frontends
  bin/godel-hook     ; called by Claude Code hooks; forwards to emacsclient
  bin/godel-probe    ; stand-in Claude binary for the pin check
  test/              ; ERT tests (test/run-tests.sh)
```

### Modes

- **`godel-mode`**: a global minor mode and the on/off switch. When on, it
  makes sure the Emacs server is running, sets up the hook receiver, the tab
  bar badges (in `my/tab-bar-workspaces`), the modeline segment and the
  `SPC G` prefix. It is a minor mode because godel works across buffers that
  have their own major modes (ghostel, magit, source files).
- **Major modes for godel's own buffers:** `godel-board-mode`,
  `godel-compose-mode`, `godel-review-mode`.
- **`godel-diff-comment-mode`**: a minor mode turned on in diff buffers that
  belong to a task.

### Task model

```elisp
(:id "fix-auth-a1b2" :title "Fix auth token refresh"
 :repo "~/code/foo" :base "master" :branch "godel/fix-auth-a1b2"
 :worktree "~/.godel/worktrees/foo/fix-auth-a1b2"
 :group nil                       ; fan-out group ID, if any
 :model "opus" :effort "high" :permission-mode "default"
 :session-id "<uuid>"             ; chosen by godel, passed with --session-id
 :state working :last-event "..." :created ... :pr-url nil
 :comments (...))                 ; review comments (see Review)
```

Stored per repo in `$(git rev-parse --git-common-dir)/godel/tasks.eld`. Every
worktree shares that directory and git never commits it. Drafts go in
`.../godel/drafts/`. The board, tab bar and modeline all read from the store
and never keep their own copy.

### Status from hooks

godel passes hooks to each session with `--settings '<json>'`. The global
`~/.claude/settings.json` is not touched, and only sessions godel starts
report back. `bin/godel-hook EVENT` reads the hook JSON on stdin and calls
`emacsclient -n --eval '(godel-status-event ...)'`. The task is found from
`cwd`, since each worktree belongs to exactly one task.

| Hook | Task state and data used |
|---|---|
| `SessionStart` | check `session_id`; record `transcript_path`; starting `model` |
| `UserPromptSubmit` | **working** |
| `PreToolUse` | **working**; `tool_name` + `tool_input` kept as the row's last activity |
| `Notification` | **needs-attention**; `notification_type` (e.g. `permission_prompt`) + `message`, shown with the last `PreToolUse` ("permission: Bash(touch …)") |
| `Stop` | **needs-review** if there is a diff vs base, otherwise **idle**; `last_assistant_message` shown on the row; sent comments become *addressed* |
| `SessionEnd` | **stopped**; `reason` |
| `PreModelSwitch` | confirms a model switch: `from_model`, `to_model`, `source` |

Other states: **setting-up** (worktree and setup script running), **merged**.

**Notifications that aren't requests.** Claude sends a `Notification` with
"Claude is waiting for your input" (`idle_prompt`) about a minute after a
finished turn. It's ignored: the task is already shown as idle or ready for
review. Permission requests, questions (`elicitation_dialog`) and unknown
types mean **needs-attention**.

**Missed events.** If Emacs isn't running, `emacsclient` fails and the event
is lost (`godel-hook` always exits 0 so Claude isn't affected). Sessions run
inside Emacs, so they end with it too. When `godel-mode` starts, it starts
the Emacs server if needed and marks tasks that were working, needed
attention or were setting up as **stopped**; idle and needs-review tasks
keep their state.

**Opening a task resumes its session.** If a task's session isn't running
(after an Emacs restart, say), opening the task resumes it with
`--resume <session-id>` on the task's model (pinned, or Not Diamond without
pins), and shows it on the right once the launch has returned. This happens
on `RET` (board or sidebar) and on any switch to the task's workspace. The
task keeps its state: a needs-review task stays needs-review (`:resume-state`,
restored at `SessionStart`); stopped tasks come back idle. Not resumed
automatically: model-mismatch tasks (use `R`), tasks still starting, tasks
whose session never started, and tasks whose worktree is gone. (Rebuilding from transcripts only
matters once sessions outlive Emacs, with the Phase 3 tmux backend.)
Every state change runs `godel-event-functions`. That hook updates the store,
redraws the board (debounced to about 200ms) and the tab bar, and sends a
desktop notification (`notifications-notify`) on needs-attention and
needs-review.

### Agent backend

Protocol: `godel-agent-start`, `-send`, `-stop`, `-buffer`, `-live-p`. The
only backend wraps `claude-code-ide`. Its sessions are keyed by project root,
and each worktree counts as its own project, so each worktree gets its own
session. Per-launch settings are applied by binding variables around the
launch call:

```elisp
(let* ((default-directory worktree)
       (process-environment (cons "GODEL_SERVER=<socket>" launch-env))
       (claude-code-ide-cli-path "notdiamond-claude")
       (claude-code-ide-cli-extra-flags
        "--session-id <uuid> --settings '<hooks json>' [--effort E] [--permission-mode M]")
       (claude-code-ide-focus-on-open nil))
  (save-window-excursion (claude-code-ide)))
```

- `launch-env` comes from `godel-models-launch-env` (see Model and effort);
  the model is chosen by the pin variables in it, never with `--model`.
- `--resume <session-id>` replaces `--session-id` when restarting a session
  that has started before (it has a transcript).
- `save-window-excursion` keeps the user's layout: claude-code-ide always
  shows a new session in a side window.
- Sessions are named `*claude-code[REPO/SLUG]*`
  (`claude-code-ide-buffer-name-function`, set by `godel-mode`), so
  same-named worktrees in different repos don't collide. claude-code-ide
  recognises its buffers by a buffer-local marker, not the name.
- Prompts are sent with `ghostel-paste-string` (bracketed paste, so
  multi-line prompts arrive whole) followed by Return.
- Stopping a session kills its buffer, which is how claude-code-ide stops
  sessions too.

**Reading the screen.** ghostel doesn't render into a buffer that isn't
displayed, and godel starts sessions hidden. So `godel-tui-screen` reads the
text from the terminal emulator itself (`ghostel--copy-all-text`, the
function behind ghostel's copy-all command) and keeps only the bottom
screenful, so an answered dialog that scrolled away can't match. Claude's
input line uses a no-break space after `❯`; the patterns accept both.

A task can also have **secondary sessions** that don't edit, such as
reviewers (Phase 2). These use `claude-code-ide` named instances in the same
directory.

### Model and effort

**What the wrapper does.** `notdiamond-claude` runs `notdiamond claude`, which
starts Claude Code behind Not Diamond's routing proxy. It removes `--model`
from the arguments and `model` from any `--settings`, and starts every
session on `notdiamond[1m]` (the router). `--effort`, `--permission-mode`,
`--session-id` and godel's `--settings` pass through.

**The model list (`godel-models.el`).** godel asks Claude for the list through
the `initialize` request of the Agent SDK's control protocol (the call behind
the SDK's `supportedModels()`):

```
notdiamond-claude -p --input-format stream-json --output-format stream-json --verbose
stdin:  {"type":"control_request","request_id":"godel-1","request":{"subtype":"initialize"}}
```

The `control_response` has `models`: the same entries as the TUI's `/model`
menu, including the wrapper's Not Diamond entry. Each has `value` (what to
pin), `displayName`, `description`, `supportedEffortLevels` and
`resolvedModel`. On 2026-11-12:

| `value` | Name | Description | `resolvedModel` |
|---|---|---|---|
| `default` | Default (recommended) | Opus 5.5 · Best for everyday, complex tasks | `claude-opus-5-5` |
| `opus` | Opus | Opus 5.5 · Best for everyday, complex tasks | `claude-opus-5-5` |
| `claude-fable-5-1[1m]` | Fable | Fable 5.1 · Most capable for your hardest and longest-running tasks | `claude-fable-5-1` |
| `sonnet` | Sonnet | Sonnet 5.5 · Efficient for routine tasks | `claude-sonnet-5-5` |
| `haiku` | Haiku | Haiku 5.5 · Fastest for quick answers | `claude-haiku-5-5` |
| `notdiamond[1m]` | Not Diamond | Routes each turn to the best model and effort | `notdiamond[1m]` |

All of them list `low, medium, high, xhigh, max` effort. The request takes
about 3s, makes no model call and creates no transcript. It runs in the
background (`make-process`, reading only lines that start with `{`, since the
wrapper prints two plain-text lines first). The result is cached and
refreshed when `claude --version` or `notdiamond --version` changes (both
auto-update), or with `godel-refresh-model-options`. Fallback: the default
value of `godel-model-options` (the table above).

**Picking.** In the new-task transient, `-m` and `-e` open completion lists
(ivy) with names and descriptions. Not Diamond is the default. Effort choices
come from the selected model's `supportedEffortLevels`. When Not Diamond is
selected, effort is greyed out because the router picks effort itself on every
turn.

**Applying the choice: the model pin.** The wrapper starts a session on a
fixed model when these are set in the session's environment:

```
NOTDIAMOND_RESEARCH_MODEL_PIN=<value>      e.g. sonnet, claude-fable-5-1[1m]
NOTDIAMOND_RESEARCH_EFFORT_PIN=<effort>
NOTDIAMOND_ROUTING=off
```

It then passes `--model <value>` to Claude, and requests go to that model as
`passthrough` without the router. Tested on 2026-11-12 with `sonnet`/high and
`claude-fable-5-1[1m]`/medium: the banner, `SessionStart`, and the transcript
all showed the pinned model. Nothing is saved to the user's settings.

**Not Diamond tasks set no pin**, so none of these variables reach them.
`NOTDIAMOND_ROUTING=off` only changes what the wrapper does for that one
launch; routing itself happens in the shared proxy, which only looks at its
own environment. Tested on 2026-11-12: with a pinned Sonnet session still
running, a Not Diamond session's requests were routed (the router picked Opus
at xhigh for the turn and Haiku at low for a side request), while the pinned
session's requests stayed `passthrough`. The one way to break this is a proxy
started *by* a pinned launch, which the proxy guard below prevents.

**Two launch paths.** Every launch goes through one function,
`godel-models-launch-env`, which builds the environment for that launch from
the chosen entry:

| Chosen model | Pin variables | `--effort` | `SessionStart` model must be |
|---|---|---|---|
| **Not Diamond** (`value` `notdiamond[1m]`) | **all three removed** | not passed (the router picks effort) | `notdiamond[1m]` |
| Any other entry | all three set | same value as the effort pin | the entry's `resolvedModel` |

The Not Diamond entry is recognised by its `resolvedModel` starting with
`notdiamond`, not by its position in the list.

**Not Diamond launches never carry the pin variables.** Removing them is
active, not just "not adding them": Emacs may have inherited
`NOTDIAMOND_RESEARCH_MODEL_PIN`, `NOTDIAMOND_RESEARCH_EFFORT_PIN` or
`NOTDIAMOND_ROUTING` from the shell that started it. For a Not Diamond launch,
godel puts the bare names (`"NOTDIAMOND_ROUTING"`, no `=`) at the front of the
let-bound `process-environment`, which unsets them for the child process. If
`godel-mode` finds any of them in Emacs's own environment at startup, it warns
once.

Launch steps:
1. **Proxy guard (every launch).** A proxy started by a pinned launch would
   inherit `NOTDIAMOND_ROUTING=off` and turn routing off for **every** session
   until it restarts. So before every launch, godel checks the proxy: first
   by asking it directly for `/stats` over HTTP (address from the notdiamond
   config, `godel-proxy-address` to override; ~0.02s), and only if that
   fails with `notdiamond status --json` (~1.2s, mostly Node start-up). If
   no proxy is running, godel starts one itself
   with `notdiamond start` in an environment with the three pin variables
   removed, and waits for `status` to report `ok`. Running it before every
   launch (not just pinned ones) means the proxy is always one godel started
   cleanly or one that was already running.
2. Launch with the environment from `godel-models-launch-env` (bound in
   `process-environment` for this launch only, never set in Emacs's own
   environment), `--session-id`, the hooks, `--effort` for pinned launches
   only, and **no prompt**.
3. If the trust dialog appears (repo never trusted), ask in the minibuffer
   "Trust <repo> for Claude?" and answer the dialog. Its default is
   "No, exit", so godel never just presses Enter. Worktrees of a trusted repo
   are trusted automatically.
4. Wait for `SessionStart` and check its `model` against the table above
   (ignoring `[1m]`-style suffixes). On a mismatch godel stops the session
   (before any prompt is sent) and marks the task **model mismatch**. For a
   Not Diamond task a mismatch means a pin leaked into the launch.
   **Resumed sessions don't report a model** in `SessionStart`. For those,
   godel checks the running processes in `/proc` instead
   (`godel-agent-check-launch`): Not Diamond sessions must have no pin
   variable in any process's environment and Claude must have been given
   `--model notdiamond…`; pinned sessions must have `--model <value>`. (The
   transcript can't be used: a routed turn records the model the router
   picked.) ghostel's native terminal gives Emacs a pipe without a PID, so
   the process tree starts from `ghostel--pid`.
5. Wait for the input box on screen, then send the prompt
   (`claude-code-ide-send-prompt`; multi-line prompts arrive whole).

`R` (restart/resume) relaunches with `--resume <session-id>` and the
environment for the newly chosen model, so changing the model or effort
mid-task needs no interaction with the TUI. Switching a task from a pinned
model to Not Diamond relaunches without the pin variables, as above. Fan-out
attempts each get their own environment, so a group can mix Not Diamond and
pinned attempts.

**Pin check (`godel-models-check-pin`).** The pin is an internal wrapper
feature, so godel tests it whenever the model list refreshes (when Claude
Code or notdiamond change version, or with `SPC G M`):
1. Proxy guard first, so the pinned run below can't start the proxy.
2. Write a temporary copy of the notdiamond config with `claudeBinaryPath`
   pointing at `bin/godel-probe`, a stand-in that records the arguments the
   wrapper passes to Claude and exits (it answers `--version` with the real
   binary). The real config is never edited.
3. Run the wrapper twice with that copy: pinned to a model, and for Not
   Diamond. The pinned run must get `--model <pin>`; the Not Diamond run must
   get `--model notdiamond…` (the resume check relies on that too).

About 4s, no Claude session, no model call. The result (`ok`, `broken`, or
`unknown` if it couldn't run) is saved with the model cache. When it's
`broken`: one warning, pinned models show as unavailable in the picker and
can't be chosen, and pinned launches are refused (also through `R` and the
API). Not Diamond keeps working.

**No picker fallback.** Driving the TUI's `/model` menu when the pin breaks
was considered and dropped (2026-11-12): it only helps in the case the pin
check already reports, it's screen-reading code that would rarely run, and
one wrong key there changes the user's default model. If the pin breaks, the
fix belongs in the wrapper (keep the pin, or add a supported flag). godel opens `/model`, finds the
target row and the `❯` cursor row on screen, sends the exact number of
Up/Down presses (the menu wraps), checks the cursor is on the target, and
presses `s` (this session only). `PreModelSwitch` confirms the switch. If that
fails too, the board marks the task **model mismatch**. godel never uses
`/model X` or `/effort X`: both save the user's default for all new sessions.

**Defaults and overrides.**
- Defaults: `godel-default-model` (Not Diamond), `godel-default-effort`,
  `godel-default-permission-mode` (nil = Claude's own default, currently
  auto mode).
- Per-repo overrides go in `.dir-locals.el`.
- Each task can set its own values in the new-task transient. The values are
  saved on the task.

## Interface

### Board (`SPC G a`)

A `magit-section` buffer grouped **by repo** (alphabetical), then by state
inside each repo. Empty state groups are hidden. Repo headings show the path
and a count per state, so a collapsed repo still shows whether it needs you.

```
godel · 5 tasks in 2 repos      n new · RET open · m message · R restart · x tear down · gr refresh

▾ doom  ~/.config/doom                                                    ●1 ◐1 ✓1
    Needs attention (1)
      ● Fix ivy multi-select     Opus/high      permission: Bash(npm test)      3m
    Working (1)
      ◐ Add tab badges           Not Diamond    Edit(config.el)                 1m
    Needs review (1)
      ✓ Hello file               Haiku/low      1 file changed, 2 untracked     5m

▾ notdiamond-code-proxy  ~/code/notdiamond-code-proxy                        ◐1 ○1
    Working (1)
      ◐ Retry 429s               Sonnet/high    Bash(go test ./...)            30s
    Idle (1)
      ○ Bump deps                Not Diamond    Done.                           2h
```

`TAB` on a repo heading collapses it (remembered across refreshes). `n` on a
repo heading creates a task in that repo; on a task, in the task's repo.

**The board has its own workspace.** `SPC G a` switches to a Doom workspace
named **Godel** (`godel-board-workspace`), creating it with the board on
first use; afterwards it just switches back there. Without Doom workspaces
the board opens in the selected window.

**Colours.** Each state has its own colour, used for the icons, the counts,
the reason text, and the tab-bar badges:

| | State | Colour |
|---|---|---|
| ● | Needs attention | red, bold |
| ✗ | Model mismatch | magenta, bold |
| ◐ | Working | yellow |
| … | Setting up | blue |
| ✓ | Needs review | green |
| ○ | Idle | normal text colour |
| ■ | Stopped | grey |

The colours are the *foreground* of the theme's `ansi-color-*` faces, so they
follow the theme (only the foreground: those faces also set a background).
The faces are `godel-state-<state>`; anything set on them with
`custom-set-faces!` takes priority. Text uses `font-lock-face` as well as
`face`, because font-lock (on in the board) strips plain `face` properties;
that's why the icons first showed up white.

### Sidebar (`SPC G o`)

The same board in a narrow window on the left, with the same keys. Compact
rows: no state sub-headings; within a repo, tasks are in state order and the
icon shows the state. The task whose workspace you're in is marked `▶`; tasks
that need you get a second line with the reason.

```
 godel                ●1 ◐2 ✓1
▾ doom
 ▶● Fix ivy multi-se…   3m
     permission: Bash(npm…)
  ◐ Add tab badges      1m
  ✓ Hello file          5m
▾ notdiamond-code-proxy
  ◐ Retry 429s         30s
  ○ Bump deps           2h
```

- Opens automatically in every task workspace (however you get there).
- `SPC G o` hides it **everywhere** until `SPC G o` again; in non-task
  workspaces `SPC G o` opens it too.
- `RET` switches to the task's workspace and the sidebar comes along; `n`
  starts a task without leaving the current one.
- Left side window, kept by `delete-other-windows`, in its own slot so it can
  share the left side with Treemacs. Claude stays on the right.

| Key | Action |
|---|---|
| `n` | New task (see below) |
| `F` | Fan-out: the same flow, with attempts > 1 |
| `RET` | Open the task's workspace (created if needed): Claude on the right, magit-status of the worktree on the left |
| `TAB` | Expand the row: last hook message, diffstat vs base, model/effort, age |
| `m` | Quick reply to the agent from the minibuffer |
| `d` | Diff vs base (`magit-diff-range base...branch`) with comment mode on |
| `v` | Review transient (see Review) |
| `R` | Restart or resume, optionally with a new model/effort |
| `P` | Ship transient: commit, push, `gh pr create`; the PR URL is saved |
| `x` | Teardown: warn about uncommitted or unpushed work, stop the session, remove the worktree, close the workspace, keep the branch, forget the task. (`x`, not `k`: `k` is "up" in evil.) |
| `gr` / `/` | Refresh (evil-collection convention) / filter by repo or state |
| `q` | Quit the board |
| Group: `c` / `w` / `J` | Compare attempts / pick a winner (tear down the rest) / ask a judge session |

### New task (`n`)

1. **Transient** with defaults filled in: `-b` base, `-m` model, `-e` effort,
   `-p` permission mode, `-n` attempts. Actions: `c` compose, `RET` quick
   prompt in the minibuffer.
2. **Compose buffer** (`godel-compose-mode`, markdown). The first line is the
   title and becomes the branch slug. The rest is the prompt, with file
   completion for `@` mentions.
3. On submit everything runs asynchronously and you **stay on the board**:
   the task shows as *setting-up*, then `git worktree add -b godel/<slug>`,
   then the setup script, then Claude starts on the chosen model and receives
   the prompt (see Model and effort), and the task becomes *working*. With attempts > 1, the same steps run once per attempt,
   creating a group.

### Compose buffers: submit and cancel

Every godel text-entry buffer (new task, comment, feedback) works like magit's
commit buffer. `godel-compose-mode`'s keymap remaps the evil commands behind
the ex commands, as with-editor does (verified in Phase 0):

| Input | Result |
|---|---|
| `:wq`, `:x`, `ZZ`, `C-c C-c` | **Submit** (`evil-save-and-close`, `evil-save-modified-and-close`) |
| `:q`, `:q!`, `ZQ`, `C-c C-k` | **Cancel** (`evil-quit`) |
| `:w` | Save the draft, keep editing |
| Submit with empty content | Cancel, with a message |

As in magit, `:q` always cancels, even after `:w`.

Each buffer visits a draft file under `.../godel/drafts/` and is auto-saved,
so nothing is lost if Emacs crashes. The compose mode derives from
`markdown-mode` but doesn't run the user's `markdown-mode-hook` (inline
images and markup hiding don't suit a draft, and images fail in a terminal
frame). Cancelled drafts are kept, and the next
`n` asks whether to restore the last one.

## Review

### Comments in diffs

- `godel-diff-comment-mode` is on in every task diff (`d` and the review
  menu). The same command also works in file buffers inside a task worktree.
- **`C`** on a line, or on a visual selection, adds a comment. In task diffs
  this overrides magit's `C` (`magit-commit-add-log`), which a review diff
  doesn't need. magit maps the diff line to the real file and line, so the
  comment is tied to `file:line-range`.
- A popup opens with the quoted lines read-only at the top and markdown
  underneath. Submit and cancel work as in compose buffers.
- The comment then shows in the diff as a fringe marker plus a box under the
  line. `]c` / `[c` move between comments. `C` on a line that already has a
  comment edits it; `D` on it deletes it.
- Comments are saved on the task, so they survive restarts. Each comment
  keeps the line text so it can be found again after the agent changes the
  file. If it can't be found, the comment is marked **outdated**.
- Comment states: pending -> sent -> addressed (after the agent's next
  `Stop`), or outdated.

### Agent reviewers

- **`v a`, ask the author**: a question to the session that wrote the code,
  which knows why it made each choice.
- **`v r`, independent reviewer**: a new secondary session with
  `--permission-mode plan`, so it can't edit. Two styles:
  - **Guided walkthrough**: an interactive session that uses
    `claude-code-ide`'s MCP tools to open files and move your cursor hunk by
    hunk while you ask questions.
  - **Structured review**: runs headless with
    `notdiamond-claude -p --output-format stream-json` in the background.
    Findings appear in `godel-review-mode` as sections you can jump to; mark
    each `k` keep or `x` drop. Kept findings join the pending comments.
- **`J`, judge** (fan-out groups): the same machinery, comparing the attempt
  branches and recommending one.

### Feedback loop

- `v l` lists every pending comment and kept finding in `godel-review-mode`.
- `v s` collects them into one feedback message
  (`1. file:lines / quoted code / comment ...`, plus an overall note) and opens
  it in a compose buffer so you can edit it. `:wq` sends it to the author
  session; the comments become *sent* and the task goes back to *working*.
- `v h` shows past review rounds.

### Ship

`P` opens a transient: commit (if needed), push, then `gh pr create --fill`.
The PR URL is saved on the task, and merged tasks move to **Merged**.

## Worktree lifecycle

- Worktrees go in `~/.godel/worktrees/<repo>/<slug>`, on branches named
  `godel/<slug>`.
- Setup and teardown scripts come from `.godel.toml` in the repo. If the file
  doesn't exist, no scripts run.
- Teardown warns about uncommitted or unpushed work, runs the teardown script,
  removes the worktree and keeps the branch (fan-out cleanup asks whether to
  delete the branches).
- Import: turn any existing git worktree into a task (not Paradev's
  worktrees).

## Performance

Agents are separate processes and keep running whatever Emacs is doing.
Measured in Phase 0 (Doom in terminal mode, see `SPIKE.md`):

- **Claude's output is small:** nothing while idle, about 2 KB/s while
  working.
- **8 hidden terminals at about 13× that rate:** Emacs at 10% CPU;
  `emacsclient` in 2ms; timers no more than 1ms late.
- **2 of them visible:** 41% CPU; timers up to 101ms late. Visible,
  fast-redrawing terminals cost the most.
- **Hook events:** one small `emacsclient` call each. Board redraws are
  debounced.
- **git and setup scripts:** always run asynchronously.

So performance doesn't require the tmux backend; it's only for durability
(Phase 3). GUI Emacs hasn't been measured.

## Phases

**Phase 0: spike.** Done 2026-11-11. Findings and measurements in
`SPIKE.md`, scripts in `spike/`. The changes it caused are already in this
plan: the model list from the SDK `initialize` request, the model pin with
its proxy guard (tested 2026-11-12), the trust dialog, with-editor-style compose buffers, rebuilding
state after missed events, and the performance numbers.

**Phase 1: MVP.** Built 2026-11-12.

What was built:
- `godel-mode` (starts the Emacs server, warns about inherited pin
  variables, names Claude buffers per task, marks dead sessions stopped,
  desktop notifications with a `message` fallback, `●N ✓N` in the mode
  line), loaded from `config.el` with `SPC G a` (board), `SPC G n` (new
  task), `SPC G M` (refresh the model list).
- Task store, worktree lifecycle with `.godel.toml` setup/teardown, proxy
  guard, the two launch paths, trust dialog handling, hook-driven status,
  model checks at start and on resume.
- New-task transient (model picker with descriptions; effort enabled only for
  pinned models, refreshed on every change with `:refresh-suffixes`),
  compose buffer with `:wq`/`:q!`, quick minibuffer prompt.
- Board with `RET`, `TAB`, `n`, `m`, `R`, `x`, `gr`, `q`; badges on task
  workspaces in the tab bar (`godel-workspace-badge`, used by
  `my/tab-bar-workspaces`).
- `godel-api-*` basics, ERT tests (`test/run-tests.sh`).

Tested end to end in Doom (terminal mode) on 2026-11-12: Not Diamond task
via the quick prompt (routed, per the proxy log); Haiku/low task via the
pickers and compose `:wq` (passthrough to Haiku, needs-review with "1
untracked"); `m` follow-up; `R` to Haiku and back to Not Diamond (resume,
process check passed, no pin variables in the Not Diamond processes); `RET`
workspace; `x` teardown keeping branches; trust dialog in a new repo; model
list refresh.

Added after the first end-to-end run: the pin check (see Model and effort),
tested against the real wrapper (ok in 3.9s, notdiamond config unchanged).
31 ERT tests in total.

Not built yet: the `/` filter on the board and the GUI performance check.
(The TUI picker fallback was dropped; see Model and effort.)

Phase 1 tests for the launch environment:
- Unit (ERT): for the Not Diamond entry, the environment has no
  `NOTDIAMOND_RESEARCH_MODEL_PIN`, `NOTDIAMOND_RESEARCH_EFFORT_PIN` or
  `NOTDIAMOND_ROUTING` with a value, **even when Emacs's own environment has
  them set**, and no `--effort` flag. For a pinned entry, all three are set and
  `--effort` matches the effort pin.
- Integration: a Not Diamond task's `SessionStart` reports `notdiamond[1m]`,
  and the proxy log shows routing decisions (not `passthrough`) for it, also
  while a pinned task is running.

**Phase 1.1: board by repo, and the sidebar.** Built 2026-11-13 (see Board
and Sidebar under Interface). `godel-sidebar.el`; the board and the sidebar
are two views drawn by `godel-board-mode` (`godel-board--compact`). Tested in
Doom (terminal mode): repo grouping, `SPC G o` in `main`, `RET` from the
sidebar (workspace switch, sidebar re-attached, `▶` and tab badge), hidden
everywhere after `SPC G o` (incl. a workspace whose saved layout had it),
re-shown in task workspaces, `n` on a task and on a repo heading. Fixed
along the way: Doom's indent guides in the sidebar (godel opts out through
`+indent-guides-inhibit-functions`), and row width leaving room for magit's
"…". 34 ERT tests.

Follow-ups after the first GUI look (2026-11-13): state colours (icons were
white because font-lock removed their faces), and the board's own "Godel"
workspace. Checked in Doom by reading the terminal's colour codes: each
state shows its own foreground on the normal background, tab badges
included; `SPC G a` from a task workspace opens and then reuses "Godel".
39 ERT tests.

Automatic resume (2026-11-13, after the first GUI restart showed sessions
didn't come back): tested with a real Haiku task through two Emacs
restarts. `RET` from the board and a direct workspace switch both resumed
the same session (`--resume <id>`, Haiku pin), showed it with the earlier
conversation, and kept needs-review; switching away and back didn't start a
second session. Found on the way: showing the buffer from a timer could run
during claude-code-ide's start-up pause and be undone by godel's
`save-window-excursion`; `godel-agent-start` now takes an ON-LAUNCH
callback. 43 ERT tests.

Duplicate sessions (2026-11-13, reported from the GUI): after a restart,
opening a task produced two godel launches of the same `--resume` in one
worktree, and claude-code-ide asked for an instance name for the second.
The exact trigger wasn't reproduced. Every path to a second launch is now
closed: one start per task (`godel-agent--starting`; a second request waits
for the first and gets its buffer), every check reads the stored task, no
launch over a live session, and `R` refuses while a task is starting.
46 ERT tests.

Faster proxy check (2026-11-13): the guard asked `notdiamond status`, which
took ~1.2s per launch; it now asks the proxy directly first (~0.02s).
Measured resume after this: `RET` to Claude window 0.45s (1.07s on the
first resume after starting Emacs, which also loads claude-code-ide, the
terminal module and magit); Claude is ready for input ~2s later, which is
Claude's own start-up. Also found: the "Method not found" (-32601) errors
come from claude-code-ide's Emacs-tools MCP server, not godel; the user is
checking upstream. 48 ERT tests.

**Phase 2: review and ship.** Diff vs base, `C` comments, pending list,
feedback via `v s`, ask-the-author, independent reviewer (walkthrough and
structured), ship transient, import an existing worktree.

Decisions (2026-11-13), which override the Review and Ship sections where
they differ:
- **Diff scope:** the working tree (uncommitted and untracked changes
  included) against the merge base with the task's base branch. Untracked
  files are shown by diffing against a temporary copy of the index with
  them added as intent-to-add (`GIT_INDEX_FILE` through a buffer-local
  `magit-git-environment`); the worktree's real index is never touched.
- **Keys in task diffs:** `C` comment (overrides `magit-commit-add-log`);
  `gn`/`gp` next/previous comment (`]`/`[` are magit's section navigation in
  evil-collection, so `]c`/`[c` can't be used). A comment is deleted from
  the review list. Findings: `a` accept, `x` drop.
- **Reviewer:** defaults to Not Diamond; model selectable per review.
  Reviewer sessions are separate from the task: headless runs use
  `--no-session-persistence` and no godel hooks; the walkthrough is a
  claude-code-ide instance named `review` (godel supplies the name, so no
  prompt) without godel hooks, so neither changes the task's state.
- **Ship:** the author drafts the commit message and PR description from
  its own conversation (`--resume <id> --fork-session -p`, so its live
  session is untouched); you edit the draft in a compose buffer; `:wq`
  commits, pushes and opens the PR with `gh`. A new **merged** state is
  set when `gh` reports the PR merged.
- `J` (judge) moves to Phase 4 with fan-out.
- Built in steps: diff and comments; feedback loop; ask the author;
  independent reviewer; ship; import an existing worktree.

Built 2026-11-13 (commits fc9f44d..4b1dfe7, 67 ERT tests). Keys: board
`d` diff, `v` review menu, `P` ship, `i` import; leader `SPC G c`
comment, `SPC G v` review menu, `SPC G i` import; in task diffs `C`,
`gn`/`gp`; in the review list `RET`, `C`, `x`, `a`, `s`. Tested in Doom
(terminal) with real Claude sessions: comments through the keys, a
feedback round the agent applied (pending -> sent -> addressed), a
question about a selected region, a structured review (one finding in
11s, accepted as a comment), a walkthrough in plan mode, an author draft
of the commit message and PR (8s), importing a worktree. Not tested for
real: `gh pr create` and the merged check against GitHub (covered with a
local bare remote and a fake `gh`). Learned on the way:
- magit's diff keys are text-property section keymaps, which beat every
  evil and minor-mode map; godel makes magit's `C` binding conditional
  instead (`magit-diff-section-map`).
- The walkthrough doesn't reliably open files in Emacs: with one small
  hunk the model explained everything at once.
- An interactive walkthrough leaves a normal transcript in the worktree's
  history; headless reviews and ship drafts leave none.

**Phase 3: durability.** On startup, offer `--resume <session-id>` for tasks
that were running. Optional tmux backend so agents keep running when Emacs
closes.

**Phase 4: extras.** Fan-out groups with compare/winner/judge, a port list
(`ss -ltnp` per task process tree), Linear issue -> task, scheduled tasks,
remote hosts through `remote-sync.el`.

## Mobile (later, not planned)

Possible because of the API and event hook. Options, from least to most
effort:
1. **ntfy.sh push + SSH.** One more function on `godel-event-functions` sends
   the push; reply by attaching to the tmux session over Tailscale. Needs the
   Phase 3 tmux backend.
2. **Slack.** One thread per task. A Socket Mode bot (no public URL) calls
   `godel-api-*` through `emacsclient`. Downside: code ends up in Slack.
3. **Web app** reachable only over Tailscale: board, task log, rendered diff,
   reply box. The most work, and the only option where reviewing diffs on a
   phone is pleasant.
Also check whether Claude Code's own Remote Control works through
`notdiamond-claude`.

## Out of scope

Chat view, in-app browser, design mode, cloud workspaces, and port
allocation.

## Risks

- **Coupling to `claude-code-ide`.** It may assume one session per project, or
  its display workarounds may get in the way (keep anti-flicker disabled). If
  so, the backend protocol lets us swap to plain ghostel + `claude` without
  changing the rest of godel.
- **Shared resources.** Worktrees don't isolate ports, databases or
  credentials. Document this; don't try to solve it in the MVP.
- **The model pin is named "research".** It looks like an internal experiment
  switch and could change or disappear in a wrapper update. The pin check
  catches that when the wrapper's version changes, before any task is
  launched; every launch is also checked (`SessionStart` model, or the
  processes on resume). Either way the failure is reported, not silent. Ask the CLI maintainers to
  keep it or to provide a supported flag (e.g. honouring `--model`); switch to
  that when it exists.
- **Routing off leaking to the proxy.** Handled by the proxy guard. If the
  guard is ever bypassed, every session loses routing until the proxy
  restarts. Check `/proc/<proxy pid>/environ` in tests, and keep a test that
  runs a Not Diamond session next to a pinned one and checks the proxy log
  for routing decisions (`spike/pin-test.sh` with model `-`).
- **Driving the TUI.** Still needed for the trust dialog (once per repo) and
  waiting for the input box. These depend on screen text, which a Claude
  Code update can change. godel checks what it sees before pressing anything
  and stops with the task needing attention on any mismatch.
- **Private functions of other packages.** godel uses
  `ghostel--copy-all-text`, `ghostel--pid` (and the pipe's
  `ghostel--native-pid`), and `claude-code-ide--display-buffer-in-side-window`.
  An update to ghostel or claude-code-ide can rename them; the tests and the
  end-to-end run would show it.
- **The `initialize` request.** Not a documented CLI command, but the Agent
  SDKs depend on it. If it fails, godel uses the cached list.
- **Wrapper behaviour.** godel relies on the wrapper passing `--effort`,
  `--session-id`, `--permission-mode` and `--settings` through and merging
  hooks. The spike scripts can re-check this after wrapper updates.
- **Comment drift.** Re-finding comments by their line text is a heuristic.
  The outdated state covers cases where it fails.
- **Too many workspaces.** Avoided by creating workspaces only when a task is
  opened; the board is the main way to navigate.
