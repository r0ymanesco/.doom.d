# godel: an agent development environment for Emacs

Status (2026-11-17): Phase 0 (spike) done, findings in `SPIKE.md`. Phase 1
(MVP), Phase 1.1 (board by repo, sidebar) and Phase 2 (review and ship)
built, with the follow-ups listed under Phases. The sections below describe
what is built; "Not built" notes mark what isn't. Written 2026-11-10.

Related documents:
- `MANUAL.md`: how to use godel.
- `REMOTE-PLAN.md`: the next step, running the agents on an always-on host
  so they don't depend on the laptop (replaces the old Phase 3). Mobile
  access is part of it.
- `SPIKE.md`: Phase 0 findings and measurements.

Keys at a glance:

| Where | Key | Action |
|---|---|---|
| Anywhere | `SPC G a` | Board, in its own "Godel" workspace |
| Anywhere | `SPC G n` | New task |
| Anywhere | `SPC G o` | Toggle the sidebar |
| Anywhere | `SPC G i` | Import an existing worktree as a task |
| Anywhere | `SPC G M` | Refresh the model list (and re-check the model pin) |
| Task diff or file | `SPC G c` | Comment on the line or region |
| Task diff or file | `SPC G x` | Delete the comment on the line (asks first) |
| Task diff or file | `SPC G v` | Review menu for that task |
| Board / sidebar | see Board | `RET` `TAB` `n` `m` `R` `x` `d` `v` `P p` `P d` `i` `gp` `gr` `q` |
| Task diff | `C`, `gn` / `gp` | Comment; next / previous comment |
| Review list | `RET` `C` `x` `a` `s` | Visit, edit, delete/drop, accept finding, send |
| Compose buffer | `:wq` / `:q!` | Submit / cancel |

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
| Effort | Chosen per task for every model, Not Diamond included; default `medium`. Pinned models get it as the effort pin and `--effort`, Not Diamond as `--effort` only, ready for effort tuning in the router. For now the router still picks effort itself on every turn: a review launched with `--effort medium` was routed to Opus at xhigh (2026-11-18). Changed 2026-11-17; before that, effort was greyed out for Not Diamond and never passed |
| Reviewer context | An independent reviewer (`v r`, `v w`) first asks the task's author for a briefing: the problem, the solution and the decisions behind it (including what you decided in the conversation), and what was tested. It's a read-only headless fork of the author's session, as in ship drafting, so the live session is untouched. The reviewer gets it as the author's account to check against the code, not to trust (2026-11-18). Skipped for a task whose session never started, or with `godel-reviewer-ask-author` nil |
| Paradev | Fully separate: do not read `.paradev/` config or import Paradev worktrees |
| Name / prefix | `godel` / `godel-`, code in `lisp/godel/` |
| Comment keys in diffs | `C` comment, `gn` / `gp` next / previous, only inside godel task diffs (`]c` / `[c` are taken by evil-collection's magit `]` / `[`) |
| Leader prefix | `SPC G` (`SPC g` stays git) |
| Board keys | Teardown is `x`, refresh `gr` (`k` is evil's "up") |
| Diff scope | Working tree, uncommitted and untracked changes included, against the merge base with the task's base branch |
| Reviewer | Not Diamond by default, model selectable; reviewers never report as the task |
| Ship | The author drafts the commit message and PR from a fork of its session; you edit; godel commits, pushes, runs `gh pr create` (`--draft` with `P d`); one tracked PR per task |
| Remote and mobile | Planned separately in `REMOTE-PLAN.md`: agents on an always-on host, the laptop and phone as clients |

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
   frontends (a laptop client, a phone; see `REMOTE-PLAN.md`) pluggable.

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
- **Major modes for godel's own buffers:** `godel-board-mode` (the board
  and the sidebar, `godel-board--compact`), `godel-compose-mode` (text
  entry), `godel-review-mode` (a task's findings and comments).
- **`godel-comment-mode`**: shows a task's review comments under their
  lines, in task diffs and in files inside a task's worktree (turned on by
  `find-file-hook`). No keys.
- **`godel-diff-comment-mode`**: on in task diffs only; adds `C`, `gn`,
  `gp`. Its keymaps are evil intercept maps, and magit's `C` binding in
  `magit-diff-section-map` is made conditional (see Review), because magit
  puts section keymaps on the diff text, which beat every other keymap.

### Task model

```elisp
(:id "fix-auth-a1b2" :slug "fix-auth-a1b2"     ; slug = workspace name
 :title "Fix auth token refresh" :prompt "the original prompt"
 :repo "~/code/foo/" :base "master" :branch "godel/fix-auth-a1b2"
 :worktree "~/.godel/worktrees/foo/fix-auth-a1b2/"
 :model "notdiamond[1m]" :effort "medium" :permission-mode nil
 ;; session
 :session-id "<uuid>"            ; chosen by godel (--session-id), updated on resume
 :transcript "/path.jsonl"       ; from SessionStart; "has started" for resume
 :pending-prompt nil             ; sent once the session is ready
 :expect-model "notdiamond[1m]" :pinned nil :model-actual "notdiamond[1m]"
 :model-check-pending nil :resume-state nil
 ;; state shown on the board
 :state needs-review :last-event "..." :last-tool "Bash(npm test)"
 :last-message "..." :diffstat "2 files changed, 1 untracked"
 :created 1794... :updated 1794...
 ;; review (Phase 2)
 :comments ((:id 1 :file "app.py" :start 4 :end 5 :anchor "the lines' text"
             :text "Why 20?" :state pending :round nil :created ...))
 :comment-seq 1
 :findings ((:id 1 :file "app.py" :start 5 :end 5 :severity "high"
             :text "..." :state proposed :created ...))   ; proposed|accepted|dropped
 :finding-seq 1 :review-status done :review-note "1 finding"
 :review-started 1794... :review-activity "Grep(foo)" :review-seen nil
 :review-rounds ((:round 1 :time ... :text "the feedback sent"))
 ;; ship
 :pr-url "https://github.com/.../pull/7" :pr-state open  ; open|closed|merged
 :pr-draft nil :ship-note nil)   ; ship-note: kept until the author can take it
```

States: `setting-up`, `needs-attention`, `model-mismatch`, `working`,
`needs-review`, `idle`, `stopped`, `merged`.

Changes to the review progress and PR fields (`godel-task--quiet-props`)
don't count as the task being updated, so they don't reset the board's age
column.

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
| `Stop` | **needs-review** if there is a diff vs base, otherwise **idle**; `last_assistant_message` shown on the row; runs `godel-status-stop-functions` (review comments that were *sent* become *addressed*) |
| `SessionEnd` | **stopped**; `reason` |
| `PreModelSwitch` | confirms a model switch: `from_model`, `to_model`, `source` |

Other states: **setting-up** (worktree and setup script running),
**merged** (set when `gh` reports the task's PR merged; see Ship).

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
pins), and shows it between the sidebar and magit once the launch has returned. This happens
on `RET` (board or sidebar) and on any switch to the task's workspace. The
task keeps its state: a needs-review task stays needs-review (`:resume-state`,
restored at `SessionStart`); stopped tasks come back idle. Not resumed
automatically: model-mismatch tasks (use `R`), tasks still starting, tasks
whose session never started, and tasks whose worktree is gone. (Sessions
that outlive Emacs are the subject of `REMOTE-PLAN.md`.)
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
  shows a new session in a window (a side window if `claude-code-ide-use-side-window`, which
  is off here: see below).
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
come from the selected model's `supportedEffortLevels`, and effort can be set
for every model, Not Diamond included; the default is `medium`
(`godel-default-effort`). For now the router still picks effort itself on
every turn. Not Diamond gets the effort so that effort tuning in the router
can use it when it lands. (Until 2026-11-17, effort was greyed out for Not
Diamond and never passed.)

Checked on 2026-11-17 with the `initialize` request (no model call): the
wrapper passes `--effort` through on a Not Diamond launch (it only strips
`--model`), and Claude starts normally. An unknown value only gets a warning
("ignoring it and using the default effort").

Checked on 2026-11-18 on a real turn: an independent review launched with
`--effort medium` on Not Diamond was served as Opus at **xhigh** for all 16
of its requests (proxy log: `routeSource` `server` once, then `reuse`,
`clientEntrypoint` `sdk-cli`). So the router ignores the flag today, as
expected. (The two briefing forks that ran beside it were routed to Haiku
at medium, which fits either an ignored flag or a coincidence; only the
review is conclusive.) The proxy log is shared by every session, so pick
the lines by time window and `clientEntrypoint`, or by `ccSessionId`.

**Tasks that already exist.** Before 2026-11-17 the router never received an
effort, so a Not Diamond task's stored `:effort` was never applied; now it is
(`--effort`). Which stored values exist: tasks made through the new-task menu
have none for Not Diamond (greyed-out options aren't part of the menu's
arguments; checked on the real task stores, 2026-11-18: no Not Diamond task
has one), and `R` stored none for it either. The one path that did store one
is `godel-import-worktree`, which saves `godel-default-effort` with the
default model: a Not Diamond task imported before the default became `medium`
has `"high"`, shown on the board as `Not Diamond/high` and passed as
`--effort high` on its next start or resume. Nobody chose it. Change it with
`R` (the router ignores it for now). There's no automatic migration: a stored
`"high"` on a Not Diamond task can't be told apart from one chosen since.

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
| **Not Diamond** (`value` `notdiamond[1m]`) | **all three removed** | the task's effort (no effort pin; for now the router picks effort itself) | `notdiamond[1m]` |
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
   environment), `--session-id`, the hooks, `--effort`, and **no prompt**.
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
model to Not Diamond relaunches without the pin variables, as above.

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
fix belongs in the wrapper (keep the pin, or add a supported flag). godel
never uses `/model X` or `/effort X`: both save the user's default for all
new sessions.

**Defaults and overrides.**
- Defaults: `godel-default-model` (Not Diamond), `godel-default-effort`
  (`medium`, for every model and for reviewers),
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
godel · 5 tasks in 2 repos      n new · RET open · m message · R restart · x tear down · P p/d ship (PR/draft) · gp open PR · gr refresh

▾ doom  ~/.config/doom                                                         ●1 ◐1 ✓1
    Needs attention (1)
      ● Fix ivy multi-select           Opus/high          permission: Bash(npm test)              3m
    Working (1)
      ◐ Add tab badges                 Not Diamond/medium Edit(config.el)   ⟳ reviewing 18s · Grep 1m
    Needs review (1)
      ✓ Hello file               #12   Haiku/low          PR opened: https://github.com/…         5m

▾ notdiamond-code-proxy  ~/code/notdiamond-code-proxy                             ◐1 ○1
    Working (1)
      ◐ Retry 429s                     Sonnet/high        Bash(go test ./...)                    30s
    Idle (1)
      ○ Bump deps                      Not Diamond/xhigh  Done.                                   2h
```

Columns: state icon, title, the PR number if the task has one (coloured by
the PR's state, see Ship), model/effort, the last event, a badge
(independent review progress or findings, see Independent reviewer; or a
ship draft in progress, see Ship), and the time since the task last changed.
The effort is shown for every model, Not Diamond included, as the session is
launched with it: the task's, else `godel-default-effort`. The column is 18
wide, enough for "Not Diamond/medium". The sidebar has no model column; the
`model:` line in a task's `TAB` details shows the same label.

Keys (the same in the sidebar):

| Key | Action |
|---|---|
| `RET` | Open the task's workspace (created if needed): magit-status of the worktree in the main window, Claude split off its left (between the sidebar and magit). If the session isn't running, it's resumed (see Status from hooks). |
| `TAB` | Expand a task (id, branch, worktree, model, changes, comments, review, last message, session) or collapse a repo |
| `n` | New task, in the repo at point (task or repo heading) |
| `m` | Message the agent from the minibuffer |
| `R` | Restart or resume the session, asking for model and effort (refused while the task is still starting) |
| `x` | Tear down: warn about uncommitted or unpushed work, stop the session, close the workspace, run the teardown script, remove the worktree, keep the branch, forget the task |
| `d` | The task's diff with comments (see Review) |
| `v` | Review menu (see Review) |
| `P p` | Ship: commit, push, open a PR (see Ship) |
| `P d` | Ship with a draft PR |
| `gp` | Open the task's PR in the browser |
| `i` | Import an existing worktree of the repo at point |
| `gr` | Refresh |
| `q` | Quit the board; in the sidebar, hide the sidebar |

`TAB` on a repo heading collapses it (remembered across refreshes).

Not built: a `/` filter. (Fan-out and its group keys are under Later ideas.)

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
| ◆ | Merged | cyan |

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
that need you get a second line with the reason. After the title, a review
badge's icon and a `⇡` for a PR (coloured like the board's `#N`).

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
  share the left side with Treemacs. Claude is not a side window (side windows on
  one side stack vertically, so it would sit under the sidebar): `claude-code-ide-use-side-window`
  is left on for extra instances (the walkthrough keeps its right side window), and config.el
  advises claude-code-ide's display function so a project's main session is split off the left
  edge of the main window area by `my/claude-code-display-left-of-main`, just right of the
  sidebar, and dedicated so magit and diffs don't replace it. (It's `display-buffer-overriding-action`,
  not a `display-buffer-alist` entry: Doom's popup module replaces that list after config.el
  loads.) Code that needs a window for magit, a diff or a file uses `godel-agent-content-window`:
  the main window is internal when Claude and magit sit side by side.

### New task (`n`)

1. **Transient** (`godel-new-task-menu`) with defaults filled in: `-b` base
   (completion over the repo's branches), `-m` model (names and
   descriptions, like `/model`), `-e` effort (the model's supported levels,
   for every model including Not Diamond; default `medium`), `-p` permission
   mode. Actions: `c` compose, `RET`
   quick prompt in the minibuffer.
2. **Compose buffer**. The first line is the title (it becomes the branch
   slug); the rest is the prompt. With only one line, it's both. The prompt
   is saved on the task (`:prompt`) for reviewers.
3. On submit everything runs asynchronously and you **stay where you are**:
   the task shows as *setting-up*, then `git worktree add -b godel/<slug>`,
   then the setup script, then the proxy guard, then Claude starts on the
   chosen model, the trust dialog is answered if Claude shows one (after
   asking you), the model is checked, and the prompt is sent; the task
   becomes *working*.

Not built: file completion for `@` mentions in the compose buffer. (Several
attempts per prompt is fan-out, under Later ideas.)

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

Each buffer visits a draft file under `.../godel/drafts/` (opened without
file-type detection, so the user's `markdown-mode-hook` doesn't run) and is
auto-saved, so nothing is lost if Emacs crashes. The compose mode derives
from `markdown-mode` but doesn't run `markdown-mode-hook` either (inline
images and markup hiding don't suit a draft, and images fail in a terminal
frame). A cancelled task prompt is kept and the next `n` offers to restore
it; other kinds (comments, questions, feedback, ship drafts) aren't offered
again.

Kinds: `task`, `comment`, `question`, `feedback`, `ship`. A comment or
question shows the code it's about **above** the text, as an overlay: it
can't be edited and isn't saved in the draft or submitted. (Read-only text
didn't work: markdown's fontification dropped the property that keeps the
end of it insertable.)

## Review

Code: `godel-review.el` (diffs, comments, review list, feedback, ask the
author), `godel-reviewer.el` (independent reviewer), `godel-ship.el`.

### The review menu (`v` on the board, `SPC G v` in a task's diff or files)

| Key | Action |
|---|---|
| `d` | Diff with comments |
| `l` | Review list (findings and comments) |
| `h` | Past feedback rounds |
| `s` | Send the pending comments to the author |
| `a` | Ask the author a question (about the selected code, if a region was active when the menu opened) |
| `r` / `R` | Structured review on Not Diamond / on a model you pick |
| `w` / `W` | Walkthrough on Not Diamond / on a model you pick |
| `P` | Ship |
| `D` | Ship with a draft PR |
| `o` | Open the PR in the browser |

### Diff

`d` shows the task's **working tree, uncommitted and untracked changes
included, against the merge base** with its base branch, in a magit diff
buffer (`magit-diff-setup-buffer <merge-base>`). Untracked files appear
because git is pointed at a temporary copy of the worktree's index
(`~/.godel/tmp/index-<id>`) in which they're added as intent-to-add: a
buffer-local `magit-git-environment` sets `GIT_INDEX_FILE`, and the copy is
rebuilt before every refresh (advice on `magit-diff-refresh-buffer`). The
worktree's real index is never touched.

Everything else in the buffer is plain magit, including keys that change the
worktree (staging, discarding). Keep that in mind when typing in a diff.

### Comments

- **Where:** `C` on a line or a visual selection in a task diff; `SPC G c`
  in a file inside a task's worktree (evil's `C`, "change to end of line",
  stays as it is in files). On a line that already has an open comment, it
  edits that comment.
- **Mapping:** in a diff, the line at point is mapped to the new file's line
  with `magit-diff-hunk-line`, so a comment is tied to `file:start-end`.
- **Writing:** a compose buffer with the code shown above; `:wq` saves.
- **Display:** under its lines, in task diffs and in files of the worktree
  (`godel-comment-mode`), with its state and lines. `gn` / `gp` move between
  them in diffs. Redrawn whenever the task changes. The text is an overlay
  `before-string` at the start of the *next* line, not an `after-string` at
  the end of the commented one: the cursor can't enter display text, and
  text after a line's end belongs to that line, so `j` got stuck on it.
  Only a comment on a buffer's last line hangs off its end. Style: a `▍`
  bar coloured by state (theme foreground: yellow pending, blue sent, green
  addressed, grey outdated), the text in the normal text colour (greyed once
  sent or done), and a grey italic state line; no boxes or backgrounds, which
  made the first version unreadable (faces `godel-comment`,
  `godel-comment-dim`, `godel-comment-meta`, `godel-comment-bar`). Every
  face list ends with `default`: Emacs merges a display string's face with
  the face of the text it's drawn in front of, and in the GUI Doom's indent
  guides give leading whitespace a stipple face, which covered the comment
  with dotted lines until nothing was left unspecified.
- **Keys and magit:** `C`, `gn`, `gp` are in `godel-diff-comment-mode`'s
  evil intercept maps. That isn't enough for `C`: magit puts section keymaps
  on the diff text, which beat every evil and minor-mode map, and
  `magit-diff-section-map` binds `C` to `magit-commit-add-log`. godel
  rebinds that entry to a menu-item with a `:filter`: godel's comment command
  in task diffs, magit's command everywhere else.
- **Stored** on the task (`:comments`), so they survive restarts. Each keeps
  the text of its lines (`:anchor`). When the file changes, a comment whose
  lines no longer match is moved to where that text is now (nearest match),
  or marked **outdated** if the text is gone.
- **States:** pending -> sent (`v s`) -> addressed (when the agent's next
  turn ends), or outdated.
- **Editing:** `C` (or `SPC G c`) on a commented line edits that comment;
  also `C` in the review list.
- **Deleting:** `SPC G x` on the line, in a task diff or a task file
  (`godel-review-delete-comment-at-point`; asks for confirmation; any shown
  comment, pending, sent or addressed; asks which if several overlap). Also
  `x` in the review list. A leader key rather than a diff key, because in a
  magit diff a wrong key can discard changes (`x` there is magit's discard).

### Review list (`v l`)

A `godel-review-mode` buffer for one task: the independent review's status,
**Reviewer findings** (proposed ones, with severity, location, code and
text), then comments grouped **Pending / Sent / Addressed / Outdated** (the
last two collapsed). Keys: `RET` open the file at the comment, `C` edit,
`x` delete a comment or drop a finding, `a` accept a finding (it becomes a
pending comment, marked `[reviewer, severity]`), `s` send, `gr` refresh.

### Feedback (`v s`) and history (`v h`)

`v s` turns the pending comments into one message:

~~~
Here's my review of your changes. Please address each point:

1. `app.py` lines 4-5:
```
<the lines as they are now>
```
<the comment>
...
~~~

You edit it in a compose buffer (add an overall note, reword, delete
points); `:wq` sends it to the author's session, resuming it first if it
isn't running and waiting for its input box. The comments become *sent*
(with the round number), the round is saved (`:review-rounds`), and they
become *addressed* when the agent's next turn ends. `v h` lists past rounds,
newest first.

### Ask the author (`v a`)

A question to the session that wrote the code, which knows why it made each
choice. If a region was active in the task's diff or file when the menu
opened, the code is shown above the question and sent with it ("About
`file` lines X-Y: ..."). Asking doesn't touch the comments.

### Independent reviewer

Reviewers never report as the task: no godel hooks, `GODEL_SERVER` unset,
so the task's state doesn't change. Default model: Not Diamond
(`godel-reviewer-model`); `R` / `W` pick one.

- **Structured review (`v r`)**: a headless run (`notdiamond-claude -p
  --output-format json --no-session-persistence`, with `Edit`, `Write`,
  `MultiEdit`, `NotebookEdit` and `Bash` disallowed) gets the task's prompt,
  the author's briefing (below) and its full diff (cut at
  `godel-reviewer-max-diff` characters) and returns findings as JSON
  between `<findings>` tags: file, lines, severity, comment. They're stored
  as proposed findings and appear in the review list. It saves no
  transcript.
- **The author's briefing.** A reviewer starting from the task's prompt and
  the diff doesn't know what you and the author talked through: why this
  approach, what you ruled out or told it to do, what was tested. So both
  reviewers first ask the author (`godel-reviewer-briefing`):
  - **How.** A headless **fork** of the author's session
    (`-p --resume <id> --fork-session --no-session-persistence`, read-only,
    `GODEL_SERVER` unset), on the author's own model and effort. It's the
    ship-draft mechanism: the live session is untouched and nothing is
    saved. It's asked for three parts: **Problem** (what was asked and why,
    including what you added, constrained or corrected along the way),
    **Solution** (what it changed and why, the decisions made in the
    conversation by either of you, alternatives rejected, what was left
    undone on purpose), and **Tested** (what it actually ran and the
    results; what it didn't or couldn't test). It's told not to paste the
    diff and to say plainly when something wasn't verified. The reply is
    used as written.
  - **How the reviewer uses it.** The briefing goes into the prompt as the
    author's account, with an instruction to check it against the code and
    to point out where the change doesn't do what it says or where the
    testing leaves a gap, not to take it on trust. This matters: the first
    real briefing (2026-11-18) attributed a design constraint to the user
    that actually came from the code's comments. Treat the briefing as a
    claim to verify.
  - **How it's written.** The request to the author is written for the
    author's own reader: it's told the request comes from the review tool
    (not the user), that the reviewer is a third party who can't see the
    conversation, that it should write about itself as "I", the person it
    worked with as "the user", and the reviewer as "the reviewer". Before
    (2026-11-18) the request was phrased as the user speaking ("what I told
    you", "you and I"), so the briefing came back addressed to the user as
    "you", and a reviewer read that "you" as itself. A test checks the
    request for the old phrasing. (Its reply is still the author's own
    words: it isn't checked, beyond the three headings.)
  - **Skipped when** the task's session never started (no transcript to
    resume), or `godel-reviewer-ask-author` is nil. The review then works
    from the task's prompt and the diff, as before.
  - **If the author can't answer** (an error, an empty reply, or no answer
    within `godel-reviewer-briefing-timeout`, default 300 s, after which
    the fork is killed, with everything it started: see below), the review
    goes ahead without the briefing and
    says so: the note reads "2 findings (the author couldn't brief it:
    <reason>)" in place of "2 findings (briefed by the author)". The reason
    is the first line of the error only, cut to 80 characters: an API error
    can run to many lines and the note and the board's details have one.
  - **Killing a hung author.** The process godel starts is the notdiamond
    wrapper (node), whose child is the real `claude`. Checked with the real
    wrapper (2026-11-18): killing the wrapper's pid alone leaves `claude`
    running, orphaned, still spending tokens 20 s later; Emacs's
    `kill-process` signals the child's whole process group (the child is its
    own group leader), and both were gone within 3 s. The timeout uses
    `kill-process`, and a test kills a real parent-and-child pair, so a change
    that only killed the parent would fail it.
  - **One question at a time.** A review and a walkthrough started together
    share one answer instead of asking twice.
  - **Seeing it.** A structured review's briefing is saved on the task
    (`:review-briefing`) and shown in the review list under "The author's
    briefing to the reviewer", collapsed until `TAB`, so you can see what
    the reviewer was told. It isn't counted as a task update
    (`godel-task--quiet-props`). It's the review's alone: a review clears
    it when it starts, and a walkthrough neither writes nor clears it, so
    the list never shows a briefing next to findings that came from another
    one. (A walkthrough's briefing is in its first message, in its chat.)
  - **Progress.** While it waits for the author, the board badge reads "⟳
    reviewing 12s · asking the author".
  - **A fresh diff.** The author can keep working while it writes the
    briefing (up to the timeout), and the reviewer reads the files as they
    are when it runs. So the diff in the prompt is taken again when the
    briefing arrives (the walkthrough's, when its first message is sent); if
    that fails, the one taken at the key press is used. (That one is still
    taken first, so a task with no merge base fails at once.) The briefing
    itself is as of the ask, so on a task that's still working it can be a
    little behind the diff. The reviewer isn't told.
  - **Cost.** One extra turn of the author's model over its whole
    conversation: about 18 s for this task, longer and dearer for a long
    task. The author is a fork, so a long transcript is read again.
  - Tested live on this task: the briefing came back in ~18 s as the three
    sections, the review's prompt carried it ahead of the diff, and the
    review ended as "6 findings (briefed by the author)". Walkthrough, same
    day: the `reviewing · asking the author` note showed for 61 s, the
    session opened 0.1 s after the briefing arrived, and its first message
    carried the briefing ahead of the diff.
- **Progress:** the run's output is streamed (`--output-format stream-json
  --verbose`); each tool call becomes the task's `:review-activity`. The
  board row shows a badge, redrawn every second while any review runs:

  ```
  ⟳ reviewing 18s · Grep(model_params)     (blue, while running)
  ◎ 2 findings                             (yellow, until each is accepted or dropped)
  ◎ no findings / ✗ review failed          (until the review list is opened)
  ```

  The sidebar shows just the icon after the title. A desktop notification
  says when it's done ("godel: review of <title> is done: N findings") or
  failed. Review progress doesn't count as a task update, so the row's age
  column isn't reset (`godel-task--quiet-props`). A review that was
  running when Emacs went away can't finish: when its task loads, godel
  marks it failed ("interrupted by a restart"), so the badge doesn't stay
  and `v r` isn't refused for good. Tested with a real review:
  the badge ticked each second, showed `Glob(**/*.py)` while the reviewer
  searched, and ended at "◎ 4 findings" with the notification.
- **Walkthrough (`v w`)**: an interactive claude-code-ide session in the
  task's worktree, an instance named `review` (godel answers
  claude-code-ide's instance-name prompt itself). It can't change files:
  `Edit`, `Write`, `MultiEdit`, `NotebookEdit` and `Bash` are disallowed.
  (Not plan mode: plan mode asks before every MCP tool call, godel's
  included.) godel sends it the author's briefing and the diff and asks it
  to go hunk by hunk, showing each location with `godel_show_location`. You
  reply in its window. **The session opens only once the briefing is
  back.** While the author writes it, the board shows `⟳ reviewing 12s ·
  asking the author`, the same note the structured review shows, and nothing
  is opened or typed into. When the briefing arrives the note clears, the
  session opens, and the first message (carrying the briefing and the diff)
  is sent once its input box is ready. If the author fails or doesn't answer
  in time, the session still opens, without the briefing, and the message
  says why. If you close the session before the first message, nothing is
  sent. The note is kept in memory, not in the task store, so a restart
  can't leave it behind.
  - **It never opens under you.** The wait is 20-60 s, and you may have
    moved on. Having waited, the session opens **without taking the focus**,
    so what you're typing stays where you're typing it, and a notification
    says "walkthrough of <title> is ready". If by then you've moved to
    another workspace than the one `v w` was pressed in, or you're in the
    minibuffer (a prompt, `M-x`), nothing is opened: the briefing is kept
    (`godel-reviewer--prepared`, in memory, good for
    `godel-reviewer-prepared-max-age` = 10 min), the notification says
    "ready to start: press v w in <workspace>", and that `v w` opens the
    session at once with the kept briefing, with the focus (you just asked),
    without asking the author again. A kept entry is used once. If the
    author had failed to brief it, that failure is kept too, so the next
    `v w` opens at once without a briefing (the notification says so) rather
    than asking again. With no author to ask there is no wait, and it opens
    with the focus as before.
    Not done, on purpose: switching to the original workspace and back.
    Doom's `+workspace-switch` overwrites `+workspace--last` (where
    `+workspace/other` goes), godel's own activation hook
    (`godel-board-on-workspace-activated`, on a timer, so it sees the
    workspace you've come back to) could resume a session you'd stopped, and
    swapping window layouts under an open prompt is the very thing this
    avoids. (An earlier version did switch; it was removed after review.
    Hiding the session until shown was also considered, and rejected:
    `godel_show_location` falls back to the selected frame's main window
    when the session isn't displayed, so the first hunks would land in
    whatever you're working on.)
  - **One walkthrough at a time.** From `v w` until the first message has
    gone in, `v w` again is refused ("already starting"): claude-code-ide
    would start a second instance, and the first message would be pasted
    into the first one twice. The guard also covers the proxy check before
    the author is asked, and every failure frees it (proxy down, the
    session won't open, an unexpected error). A step that never answers
    (`notdiamond status`/`start` have no timeout) can't hold it forever
    either: each attempt has a token and a deadline
    (`godel-reviewer-briefing-timeout` + 180 s); after that `v w` works
    again, and the abandoned attempt, if it ever wakes up, does nothing and
    can't free the new one's guard. Once the session is open,
    `v w` shows it instead of starting another (checked live against a
    real session).
  - **It shows each hunk** with `godel_show_location(file, start_line,
    end_line?)`: the file opens in the main window beside the walkthrough,
    the lines highlighted (`godel-reviewer-focus`), your cursor stays in the
    walkthrough. (claude-code-ide's `openFile` isn't a tool the model can
    call: it's part of the IDE connection Claude Code itself uses.) The tool
    answers at once and opens the file on a timer, so a prompt from opening
    it (a Python venv y/n, lsp's import-project question) waits for you
    instead of timing out Claude's tool call.
  - **It records review comments** with godel MCP tools, registered on
    claude-code-ide's emacs-tools server (`claude-code-ide-make-tool`):
    `godel_add_comment(file, start_line, end_line?, comment)` adds a pending
    comment marked `[walkthrough]`; `godel_list_comments()` lists the open
    ones so it doesn't add duplicates. It adds comments **on its own** for
    real problems, and when you ask or agree. They go to your review list;
    you decide what's sent to the author (`v s`).
  - The tools only work for a task's `review` session: they check the
    calling session's buffer through claude-code-ide's session context and
    refuse anyone else (the author's session, other Claude sessions).
    They're pre-approved (`--allowedTools`), so there's no prompt.
  - They need claude-code-ide's MCP tools server
    (`claude-code-ide-emacs-tools-setup`); godel says so if it's off. Until
    claude-code-ide answers Claude's `server/discover` probe, the tools
    connect a few seconds after the session starts (Claude waits).
  - Tested with real walkthroughs: no prompts and no timeouts; it
    highlighted each hunk beside the chat, added two comments on its own (a
    missing space, `== None`) and a third when asked in the chat. There's no
    tool to delete comments: that's the review list's `x` or `SPC G x`.
  - Unlike headless runs, it leaves a normal transcript in the worktree's
    Claude history.

### Ship (`P p`, `P d`)

`P p` opens a PR, `P d` a **draft** PR (`gh pr create --draft`); in the
review menu, `P` and `D`.

1. **Draft.** The author drafts the commit message and PR description from
   its own conversation: a headless run of a **fork** of its session
   (`--resume <id> --fork-session -p --no-session-persistence`, read-only),
   so the live session is untouched and nothing is saved. A task whose
   session never started gets a draft from the diff alone. If drafting
   fails, you get an empty draft and the reason in the header.
   - **Progress.** The compose buffer opens at once (`P` doesn't leave you
     waiting on the echo area): until the draft arrives it shows "⟳ The
     author is drafting the commit message and PR… 12s", is read-only and
     refuses `:wq` (`godel-compose-wait`; `godel-compose-fill` puts the
     draft in and switches to insert state). The board row shows "⟳
     drafting the ship message 12s" in the badge column. A notification
     says when the draft is ready. `:q!` while drafting stops the Claude
     run. The drafting table (`godel-ship--drafting`) lives only in memory,
     so a restart can't leave a stuck badge. Tested live: a real draft took
     ~20s.
2. **Edit.** A compose buffer with the commit message on top, then the line
   `==== Pull request: title on the next line, then the description ====`,
   then the PR title and description. If the task already has a PR, only
   the commit message is drafted.
3. **`:wq`** stages and commits (if there are changes), pushes
   (`git push --set-upstream origin <branch>`, `godel-ship-remote`), and
   runs `gh pr create --base <base> --head <branch> --title ... --body-file
   ...` unless a PR exists. The PR URL is saved on the task. Every command
   and its output go to `*godel-ship: <id>*`; a failure stops there and is
   shown on the board.
4. **Tell the author.** godel sends the author's session a note: "For your
   information, no action needed: I shipped this task… committed <hash>
   ("<subject>"), pushed <branch>…,
   opened a pull request: <url>" (or "which updates the pull request"),
   asking for a one-word reply. So if you keep working with it, it knows
   what was shipped. If the session isn't running (e.g. right after an
   Emacs restart) or is busy, the note is kept on the task (`:ship-note`)
   and sent when the session next starts or finishes a turn, once its input
   box is ready; not while it's waiting on you or stopped for the wrong
   model. A newer ship's note replaces an undelivered one. Tested live: a
   kept note reached a session as it started, which answered "Noted."
   (Drafting in the live session instead was considered and dropped: the
   session must be idle, read-only can't be enforced, and it can collide
   with what you're typing.)

**Finding the PR:** the board row shows `#<number>` after the title (green,
grey while a draft, cyan once merged, red if closed without merging), the
sidebar row a `⇡` in the same colours (next to the review badge, two
columns taken from the title); `TAB` shows a `PR` line
with the URL; `gp` on a board or sidebar row, or `o` in the review menu
(`SPC G v`), opens it in the browser (`godel-ship-browse-pr`).

**PR state:** `godel-ship-check-merged` runs `gh pr view <url> --json
state` for tasks whose PR isn't merged: when the board opens, on `gr`, and
every `godel-ship-check-interval` seconds (default 300) while `godel-mode`
is on. The state is saved as `:pr-state` (`open`, `closed`, `merged`).
- **Merged** moves the task to **merged** (cyan). Merged tasks aren't torn
  down automatically.
- **Draft:** the check also reads `isDraft` (`:pr-draft`). An open draft
  draws the marker grey and the `PR` line says "(draft)". Changing it on
  GitHub is noted ("PR marked ready for review", "PR converted to draft").
  Shipping onto an open PR matches the key: `P p` on a draft pushes, then
  `gh pr ready`; `P d` on a ready PR pushes, then `gh pr ready --undo`.
- **Closed without merging** leaves the task's state alone, notes "PR
  closed without merging", and draws the PR marker red (`#N` on the board,
  `⇡` in the sidebar, "(closed without merging)" on the `PR` line).
  Reopened on GitHub: "PR reopened", green again.
- **One PR per task.** A task tracks the PR godel opened. Shipping again
  pushes to the branch, which updates that PR while it's open. Once it's
  closed or merged, `P` drafts a PR again and opens a new one, which
  becomes the task's PR (a merged task goes back to needs-review). PRs
  opened outside godel (github.com, or the author running `gh`) aren't
  tracked.

Confirmed against GitHub by the user (2026-11-16): opening PRs and draft
PRs, marking ready, and the state checks. Also covered by tests with a local
bare remote and a fake `gh`.

## Worktree lifecycle

- Worktrees go in `~/.godel/worktrees/<repo>/<slug>`, on branches named
  `godel/<slug>`.
- Setup and teardown scripts come from `.godel.toml` in the repo. If the file
  doesn't exist, no scripts run.
- Teardown warns about uncommitted or unpushed work, runs the teardown script,
  removes the worktree and keeps the branch.
- **Import** (`i` on the board, `SPC G i`): lists the repo's worktrees that
  aren't tasks yet, skipping the main checkout, detached worktrees and
  Paradev's (`godel-import-exclude-regexp`, `/.paradev/` paths); asks for
  the base branch and a title; makes a task with the worktree's directory
  name as its slug. It starts without a session (`R` starts one, with a new
  session ID) and as *needs review* if it already has changes against the
  base.
- Teardown of an imported task removes its worktree like any other.

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

So performance doesn't need tmux; the tmux backend in `REMOTE-PLAN.md` is
for running agents on a host. GUI Emacs hasn't been measured.

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
- New-task transient (model picker with descriptions; effort for every
  model, Not Diamond included),
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
  them set**; the effort is passed as `--effort` only. For a pinned entry, all
  three are set and `--effort` matches the effort pin.
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

**Phase 2: review and ship.** Built 2026-11-13; described in Review, Ship
and Worktree lifecycle. Decisions made for it (2026-11-13):
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
- `J` (judge) moves to the fan-out idea (see Later ideas).
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
- The walkthrough didn't reliably open files in Emacs: its prompt named
  claude-code-ide's `openFile`, which the model can't call. Fixed later
  with `godel_show_location` (see Phase 2 follow-ups).
- An interactive walkthrough leaves a normal transcript in the worktree's
  history; headless reviews and ship drafts leave none.

**Phase 2 follow-ups.** Built 2026-11-13 to 2026-11-16 from GUI use, each
tested in Doom with real sessions where it touches Claude (84 ERT tests at
the end):
- Comments: drawn under their line with a coloured bar so `j` moves past
  them and they stay readable (and not covered by indent guides);
  `SPC G x` deletes one.
- Independent review progress on the board (badge with elapsed time and
  the reviewer's current tool), and a notification when done.
- The walkthrough records comments (`godel_add_comment`,
  `godel_list_comments`, on its own and when asked) and shows each hunk in
  Emacs (`godel_show_location`).
- Ship: the compose buffer opens at once with drafting progress, plus a
  board badge; the PR shows on the board (`#N`), sidebar (`⇡`) and `TAB`
  details, `gp` / `o` open it; PR state tracking (open, draft, closed,
  merged) on board open, `gr` and a timer; `P p` / `P d` for PR or draft,
  matching an existing PR's draft state; a new PR after a closed or merged
  one; a note to the author after each ship, kept until its session can
  take it.

**Next: remote host.** See `REMOTE-PLAN.md`. It replaces the old Phase 3
(a local tmux backend) and includes mobile access.

**Later ideas (not planned):**
- **Fan-out:** start several attempts at one prompt (different models or
  efforts, each its own task), then compare them, pick a winner, or have a
  judge model choose. Each attempt gets its own launch environment, so a
  group can mix Not Diamond and pinned attempts. Board keys were sketched
  as `F` (fan out), `c` / `w` / `J` (compare, winner, judge).
- **Port list:** show the ports each task's processes listen on (`ss
  -ltnp` over the task's process tree), since worktrees don't isolate
  ports.
- **Linear issue → task:** start a task from an issue.
- **Scheduled tasks.**

## Out of scope

Chat view, in-app browser, design mode, and port allocation. Running agents
off the laptop is planned in `REMOTE-PLAN.md`, not as cloud workspaces.

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
- **Private functions and keymaps of other packages.** godel uses
  `ghostel--copy-all-text`, `ghostel--pid` (and the pipe's
  `ghostel--native-pid`), `claude-code-ide--display-buffer-in-side-window`
  and `claude-code-ide--read-instance-name` (overridden to name the
  walkthrough session), rebinds `C` in `magit-diff-section-map` (with a
  filter, so it's unchanged outside task diffs), and advises
  `magit-diff-refresh-buffer`. An update to ghostel, claude-code-ide or
  magit can break these; the tests and an end-to-end run would show it.
- **Typing in a task diff.** It's a magit diff: keys like stage and discard
  act on the worktree immediately. godel only adds `C`, `gn`, `gp`.
- **`--fork-session` and `gh`.** Ship relies on Claude's `--fork-session`
  and on `gh` being installed and logged in (`gh pr ready --undo` needs a
  recent `gh` and a plan that supports drafts). The automated tests use a
  fake `gh`.
- **The ship note starts a turn.** Telling the author what was shipped is
  a prompt, so the author replies (one word) and the task briefly shows as
  working.
- **The `initialize` request.** Not a documented CLI command, but the Agent
  SDKs depend on it. If it fails, godel uses the cached list.
- **Wrapper behaviour.** godel relies on the wrapper passing `--effort`,
  `--session-id`, `--permission-mode` and `--settings` through and merging
  hooks. The spike scripts can re-check this after wrapper updates.
- **Comment drift.** Re-finding comments by their line text is a heuristic.
  The outdated state covers cases where it fails.
- **Too many workspaces.** Avoided by creating workspaces only when a task is
  opened; the board is the main way to navigate.
