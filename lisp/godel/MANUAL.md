# godel manual

godel turns Doom Emacs into an agent development environment. You give a
coding agent (Claude Code, through `notdiamond-claude`) a **task**; godel
gives it its own git worktree and branch, starts the session, tracks what
it's doing on a board, and helps you review its changes, send feedback and
ship a pull request.

This manual covers what is built. The design and its reasoning are in
`PLAN.md`; running agents on a remote host (and from a phone) is planned in
`REMOTE-PLAN.md`.

Contents: [Setup](#setup) · [Concepts](#concepts) · [A typical
task](#a-typical-task) · [The board](#the-board) · [The
sidebar](#the-sidebar) · [Starting a task](#starting-a-task) · [Working
with a task](#working-with-a-task) · [Reviewing](#reviewing) ·
[Shipping](#shipping) · [Compose buffers](#compose-buffers) ·
[Configuration](#configuration) · [Troubleshooting](#troubleshooting) ·
[Limitations](#limitations) · [Key reference](#key-reference)

## Setup

### Requirements

- **Doom Emacs** with workspaces, evil, magit, transient, and
  `claude-code-ide` on the ghostel terminal. claude-code-ide's Emacs tools
  server must be on (`claude-code-ide-emacs-tools-setup`) for the
  walkthrough reviewer's tools.
- **`notdiamond-claude`** on your PATH, logged in. godel starts every
  session through it.
- **The real Claude Code binary** at `~/.local/bin/claude`, set as
  `claudeBinaryPath` in `~/.notdiamond/config.json`, so sessions don't go
  through another wrapper.
- **git**, and **`gh`** logged in (for shipping PRs).

### Loading it

godel lives in `lisp/godel/` of the Doom config and is loaded from
`config.el`:

```elisp
(add-to-list 'load-path (expand-file-name "lisp/godel" doom-user-dir))
(use-package! godel
  :commands (godel-board godel-new-task godel-sidebar-toggle godel-review-comment
             godel-review-delete-comment-at-point
             godel-review-menu godel-import-worktree godel-refresh-model-options)
  :config (godel-mode 1))
```

plus the `SPC G` keys (see [Key reference](#key-reference)). `godel-mode`
turns on the first time you use a godel command. It starts the Emacs server
(sessions report back through it), loads your tasks, and adds the `●N ✓N`
mode-line count.

### Per-repo setup scripts (optional)

A `.godel.toml` in a repo's main checkout gives commands to run in each new
worktree, and when a task is torn down:

```toml
setup = ["npm install", "cp ../.env .env"]   # or a single string
teardown = "docker compose down"
```

Only this subset of TOML is read: one `key = value` per line, a string or a
list of strings, `#` comments. No file, no scripts. Output goes to a
`*godel-setup: <task>*` buffer.

## Concepts

**A task** is one piece of work for one agent: a title, a prompt, a git
worktree on its own branch (`godel/<slug>`), and a Claude session in that
worktree. One task, one worktree, one branch, one author session, so agents
can work in parallel without touching each other's files.

**Where things live:**
- Worktrees: `~/.godel/worktrees/<repo>/<slug>/`.
- Tasks: in the repo's git directory (`.git/godel/tasks.eld`), shared by
  all its worktrees and never committed. Drafts of what you type are next
  to it in `godel/drafts/`.
- Each task opens in its own Doom workspace, named after its slug. Closing
  the workspace doesn't stop the agent.

**States.** Claude Code reports what it's doing through hooks, and the board
shows it:

| Icon | State | Colour | Meaning |
|---|---|---|---|
| `●` | Needs attention | red | The agent is waiting on you: a permission prompt or a question |
| `✗` | Model mismatch | magenta | The session started on a different model than the task's; it was stopped before getting your prompt |
| `◐` | Working | yellow | The agent is working |
| `…` | Setting up | blue | Creating the worktree, running setup, starting Claude |
| `✓` | Needs review | green | The agent finished a turn and the worktree has changes |
| `○` | Idle | normal text | The agent finished a turn with no changes |
| `■` | Stopped | grey | The session isn't running (e.g. after an Emacs restart) |
| `◆` | Merged | cyan | The task's PR was merged |

You get a desktop notification when a task needs attention, needs review,
or has a model mismatch.

**Models.** Each task runs on one model and effort, chosen when you create
it: **Not Diamond** (the default; the router picks a model for every turn)
or a fixed model (Opus, Fable, Sonnet, Haiku…). Effort defaults to medium.
Not Diamond is given the effort too, but for now the router still picks the
effort for every turn itself. godel applies the choice when it starts the
session and checks that the session really runs on the chosen model.

## A typical task

1. `SPC G n`, press `c`, write the title on the first line and the prompt
   below it, `:wq`. You stay where you are; the task appears on the board
   as setting up, then working.
2. `SPC G a` shows the board. When the task turns **needs review** (and you
   get a notification), `RET` opens it: the sidebar on the far left, then the
   Claude session, then the worktree's magit status.
3. `d` (on the board) or `SPC G v d` (in the task) shows the diff. `C` on a
   line or selection adds a comment.
4. `SPC G v s` sends your comments to the agent as one message. It works
   on them; the comments become *addressed* when it's done.
5. Optionally `SPC G v r` for an independent review by another model, or
   `SPC G v w` to have a reviewer walk you through the change.
6. `P p` on the board (or `SPC G v P`) ships: the agent drafts the commit
   message and PR, you edit, `:wq` commits, pushes and opens the PR.
7. When the PR is merged, the task turns cyan. `x` tears it down (the
   branch is kept).

## The board

`SPC G a` opens the board in its own workspace, **Godel**; afterwards it
switches back to it. Tasks are grouped by repo (alphabetically), then by
state. A repo heading shows its path and a count per state, so a collapsed
repo still shows when something needs you.

```
godel · 5 tasks in 2 repos      n new · RET open · … · gr refresh

▾ doom  ~/.config/doom                                                         ●1 ◐1 ✓1
    Needs attention (1)
      ● Fix ivy multi-select           Opus/high          permission: Bash(npm test)              3m
    Working (1)
      ◐ Add tab badges                 Not Diamond/medium Edit(config.el)   ⟳ reviewing 18s · Grep 1m
    Needs review (1)
      ✓ Hello file               #12   Haiku/low          PR opened: https://github.com/…         5m
```

A row shows: the state icon, the title, the PR number (if the task has one),
the model and effort (for Not Diamond too, e.g. `Not Diamond/medium`), the
last thing that happened (the tool the agent is using, the reason it needs
you, its last message…), a badge, and how long ago the task last changed.
In the sidebar, the model and effort are on the `model:` line of a task's
`TAB` details.

**Badges:**

| Badge | Meaning |
|---|---|
| `⟳ reviewing 18s · Grep(…)` | An independent review is running (what the reviewer is doing) |
| `◎ 2 findings` | Reviewer findings waiting to be accepted or dropped |
| `◎ no findings` / `✗ review failed` | Until you open the review list |
| `⟳ drafting the ship message 12s` | The agent is drafting the commit message and PR |

**The PR number** is coloured by the PR's state: **green** open, **grey**
draft, **cyan** merged, **red** closed without merging.

**Keys** (the same in the sidebar):

| Key | Action |
|---|---|
| `RET` | Open the task (its workspace: sidebar, Claude, then magit, left to right); resumes the session if it isn't running |
| `TAB` | Expand a task (id, branch, worktree, model, changes, PR, comments, review, last message, session), or collapse a repo |
| `n` | New task, in the repo at point |
| `m` | Send the agent a message (typed in the minibuffer) |
| `R` | Restart or resume the session, choosing the model and effort again |
| `x` | Tear down the task (see [Tearing down](#tearing-down)) |
| `d` | The task's diff, with comments |
| `v` | Review menu |
| `P p` | Ship: commit, push, open a PR |
| `P d` | Ship with a draft PR |
| `gp` | Open the task's PR in the browser |
| `i` | Import an existing worktree of the repo at point as a task |
| `gr` | Refresh, and ask GitHub about the tasks' PRs |
| `q` | Quit (in the sidebar: hide it) |

**Elsewhere:** the tab bar shows a coloured state badge on each task's
workspace, and the mode line shows `●N` (tasks needing you) and `✓N` (ready
for review).

## The sidebar

`SPC G o` shows the board in a narrow window on the left, with the same
keys. It opens by itself in every task workspace. `SPC G o` hides it
everywhere until you press `SPC G o` again.

```
 godel                ●1 ◐2 ✓1
▾ doom
 ▶● Fix ivy multi-se…   3m
     permission: Bash(npm…)
  ◐ Add tab badges    ⟳ 1m
  ✓ Hello file        ⇡ 5m
```

- `▶` marks the task you're in.
- Tasks that need you get a second line with the reason.
- After the title: a review badge's icon, and `⇡` if the task has a PR
  (coloured like the board's PR number).
- `RET` switches to another task; `n` starts a task without leaving this
  one.

## Starting a task

`SPC G n` anywhere, or `n` on the board (in the repo at point). Outside a
repo, godel asks which repo.

**The new-task menu:**

| Key | Option |
|---|---|
| `-b` | Base branch (completion over the repo's branches) |
| `-m` | Model, with descriptions (the same list as Claude's `/model`) |
| `-e` | Effort (the levels the model supports; default medium). Not Diamond gets it too, though for now the router picks effort itself |
| `-p` | Permission mode (default: Claude's own) |
| `c` | Compose the prompt in a buffer |
| `RET` | Quick prompt in the minibuffer |

**In the compose buffer,** the first line is the title (it also names the
branch and workspace); the rest is the prompt. A single line is both. `:wq`
creates the task, `:q!` cancels; a cancelled prompt is offered back next
time.

**What happens next,** without blocking Emacs: the worktree and branch are
created, the setup script runs, Claude starts on the chosen model, the
model is checked, and the prompt is sent. The first time Claude runs in a
repo it asks whether to trust the folder; godel asks you in the minibuffer
("Trust <repo> for Claude?") and answers for you.

**The model list** comes from Claude itself and refreshes when Claude Code
or notdiamond update. `SPC G M` refreshes it by hand. If the mechanism that
pins a model stops working, godel warns once and fixed models can't be
picked; Not Diamond keeps working.

**Importing a worktree** (`i` on the board, `SPC G i`): turns an existing
worktree of the repo into a task. godel lists the ones that aren't tasks
yet (not the main checkout, detached worktrees, or Paradev's), asks for the
base branch and a title, and adds it without a session; `R` starts one.

## Working with a task

**Opening it.** `RET` on the board or sidebar, or switching to its
workspace. If the session isn't running (after an Emacs restart, say), it's
resumed with its conversation, on the task's model. It takes a couple of
seconds. The Claude window sits between the sidebar and magit; `SPC c w` toggles its width.

**Talking to the agent.** Type in its window as usual, or `m` on the board
for a one-line message without opening the task.

**Needs attention** means the agent is waiting on you, usually a permission
prompt. Open the task and answer it in the Claude window.

**Changing the model or effort.** `R` on the board asks for a model and
effort, then restarts the session with them, keeping the conversation.

**After an Emacs restart,** sessions are gone (they run inside Emacs).
Tasks that were working or waiting become **stopped**; ready-for-review and
idle tasks keep their state. Opening a task resumes it.

### Tearing down

`x` on the board. godel warns if the worktree has uncommitted or unpushed
work, then stops the session, closes the workspace, runs the teardown
script, removes the worktree and forgets the task. **The branch is kept.**

## Reviewing

Everything is in the **review menu**: `v` on the board, or `SPC G v` in a
task's diff or files.

| Key | Action |
|---|---|
| `d` | Diff with comments |
| `l` | Review list (findings and comments) |
| `h` | Past feedback rounds |
| `s` | Send the pending comments to the agent |
| `a` | Ask the agent a question (about the selected code, if you had a selection) |
| `r` / `R` | Independent review on Not Diamond / on a model you pick |
| `w` / `W` | Walkthrough on Not Diamond / on a model you pick |
| `P` / `D` | Ship with a PR / a draft PR |
| `o` | Open the PR in the browser |

### The diff

`d` shows **everything the task changed against the point where it branched
off its base**: committed, uncommitted and new (untracked) files. It's a
normal magit diff buffer.

> **Careful:** it's real magit, so keys like `s` (stage) and `x` (discard)
> act on the task's worktree immediately.

### Comments

- **Add:** `C` on a line, or on a visual selection, in a task diff. In a file
  inside a task's worktree, use `SPC G c` (`C` stays evil's "change to end
  of line" there). Write the comment in the compose buffer (the code is
  shown above it) and `:wq`.
- **See:** comments are drawn under their lines, with a coloured bar, in
  the task's diff and in its files. `gn` / `gp` jump to the next / previous
  comment in a diff.
- **Edit:** `C` (or `SPC G c`) on a commented line.
- **Delete:** `SPC G x` on the line (asks first), or `x` in the review
  list.
- **States:** **pending** (yellow bar) → **sent** (blue) when you send them
  → **addressed** (green) when the agent's next turn ends. If the code a
  comment points at changes, the comment follows it; if the code is gone,
  it becomes **outdated** (grey).

### Sending feedback

`v s` gathers the pending comments into one message ("Here's my review of
your changes. Please address each point: 1. `file` lines 4-5: …"). Edit it
as you like (add an overall note, reword, remove points) and `:wq` to send
it to the agent. If its session isn't running, it's resumed first. `v h`
lists past rounds.

**Asking** (`v a`) sends a question instead, without touching the
comments: select code in the diff or a file first and the question is about
that code.

### The review list

`v l`: the independent review's status, the author's briefing to the reviewer
(collapsed; `TAB` shows it) and the findings, then your comments grouped
Pending / Sent / Addressed / Outdated.

| Key | Action |
|---|---|
| `RET` | Open the file at the comment or finding |
| `C` | Edit a comment |
| `x` | Delete a comment, or drop a finding |
| `a` | Accept a finding: it becomes a pending comment |
| `s` | Send the pending comments |
| `gr` | Refresh |

### Independent review

`v r` asks another model (Not Diamond unless you use `v R`) to review the
task's prompt and diff. It can read the code but not change it, and it
doesn't affect the task's state. The board shows its progress (`⟳
reviewing 18s · Grep(…)`), and you get a notification when it's done.
Findings (file, lines, severity, comment) appear in the review list as
proposals; `a` turns one into a pending comment, `x` drops it.

**The reviewer is briefed by the author.** On its own, a reviewer only sees
the task's prompt and the diff, so it knows nothing of what you and the
author talked through. So it first asks the author, which still has that
conversation, for a short briefing: the **problem**, the **solution** and
the decisions behind it (including what you decided), and **what was
tested**. While it waits, the board says `⟳ reviewing 12s · asking the
author`; this takes about 20 seconds, more for a long conversation. The
author answers from a read-only copy of its session, so the session you're
working in isn't touched. The reviewer treats the briefing as the author's
account to check against the code, not as fact: it's told to point out
where the change doesn't do what the briefing says, or where the testing
leaves a gap.

- The finished review says whether it had the briefing: `2 findings
  (briefed by the author)`.
- To see what the reviewer was told, open the review list (`v l`) and `TAB`
  on "The author's briefing to the reviewer". (That's for `v r`; a
  walkthrough's briefing is in its first message.)
- A task whose session never started has no one to ask, so it's reviewed from
  the prompt and diff alone. If the author doesn't answer (an error, or
  nothing within five minutes), the review goes ahead without the briefing
  and the note says why, in one line: `2 findings (the author couldn't brief
  it: …)`.
- The diff the reviewer gets is taken again when the briefing arrives, so it
  matches the files it reads even if the author kept working meanwhile. The
  briefing itself is as of the moment you asked.
- If Emacs restarts while a review is running, it's marked failed
  ("interrupted by a restart") and you can start it again.
- The author's answer is only as good as its memory: check anything in it
  that matters. It's written as the author's own account: the author calls
  itself "I" and you "the user", so a reviewer reads it as yours.
- `godel-reviewer-ask-author` set to `nil` turns this off.

### Walkthrough

`v w` (or `v W` to pick the model) opens a second Claude session in the
task's worktree (named `:review`) that walks you through the change, one
hunk at a time:

- For each hunk it **shows the code in Emacs**: the file opens in the main
  window with the lines highlighted, and your cursor stays in the chat.
- It explains the change, points out problems, and waits for you before
  the next hunk. Reply in its window: ask questions, disagree, ask it to
  move on.
- It **records review comments**: on its own for real problems, and when
  you ask ("note that") or agree. They're marked `[walkthrough]` and land
  as pending comments, so you still decide what's sent to the author.
- It can't edit files or run commands.

It gets the author's briefing too (see Independent review). While the
author writes it, the board shows `⟳ reviewing 12s · asking the author`, as
it does for a review, and the session doesn't open yet. When the briefing is
back, the session opens and its first message is sent.

Because that can take up to a minute, the session opens **without taking the
focus**, so what you're typing isn't redirected into it, and a notification
tells you when it's ready. If you've gone to another workspace, or are in a
prompt (`M-x`), by then, godel opens nothing under you: it keeps the briefing
and the notification says "ready to start" (a briefing that failed is kept as
a failure, so it isn't asked for again). Press `v w` in the workspace you
started in and the session opens at once (the briefing is kept for ten
minutes: `godel-reviewer-prepared-max-age`). `v w` again while it's starting
is refused; once it's open, `v w` shows the session instead of starting
another.

If opening a file asks you something (a Python environment, lsp's
"import project?"), answer it; the walkthrough isn't held up.

## Shipping

`P p` on the board (`SPC G v P`) ships with a **PR**; `P d` (`SPC G v D`)
with a **draft PR**.

1. **The agent drafts** the commit message and PR description from its own
   conversation (in a copy of its session, so its live session isn't
   touched). The ship buffer opens at once and shows "⟳ The author is
   drafting the commit message and PR… 12s"; the board row shows it too.
   It usually takes 10–30 seconds; you get a notification when it's ready.
   `:q!` while it drafts cancels.
2. **You edit.** The commit message is on top, then a separator line, then
   the PR title (one line) and description:

   ```
   Move default SAP models to gpt-5.1

   The gpt-5 models are being deprecated…

   ==== Pull request: title on the next line, then the description ====
   Move default SAP models to gpt-5.1

   ## What changed
   …
   ```

   If the task already has an open PR, only the commit message is
   drafted.
3. **`:wq`** stages everything, commits (if there are changes), pushes the
   branch to `origin`, and opens the PR with `gh` (or, with an open PR
   already, just pushes, which updates it). Each command and its output
   are in a `*godel-ship: <task>*` buffer; if one fails, the board row says
   which.
4. **The agent is told** what was shipped (commit, PR link), so it knows if
   you keep working with it. If its session isn't running or is busy, the
   note waits and is sent when the session next starts or finishes a
   turn. It replies "Noted.", which briefly shows the task as working.

**Shipping again onto an open PR follows the key:** `P p` on a draft PR
pushes and marks it ready for review; `P d` on a ready PR pushes and
converts it back to a draft.

**Finding the PR:** the PR number on the board row, `⇡` in the sidebar, a
`PR` line in the `TAB` details; `gp` on a row (or `o` in the review menu)
opens it in the browser.

**PR state** is checked with `gh` when the board opens, on `gr`, and every
5 minutes:
- **Merged:** the task turns cyan (state *merged*). It isn't torn down for
  you.
- **Closed without merging:** the PR number turns red, the row says "PR
  closed without merging". The task itself is unchanged.
- **Draft ↔ ready** changes made on GitHub are picked up (grey ↔ green).
- **Reopened:** green again.

**One PR per task.** godel tracks the PR it opened. Once that PR is closed
or merged, the next ship opens a new PR, which becomes the task's PR (a
merged task goes back to needs review). PRs opened elsewhere (on GitHub, or
by the agent running `gh`) aren't tracked.

## Compose buffers

Every text godel asks you for (a task prompt, a comment, feedback, a
question, a ship message) is written in a compose buffer that works like
magit's commit buffer:

| Keys | Action |
|---|---|
| `:wq`, `:x`, `ZZ`, `C-c C-c` | Submit |
| `:q`, `:q!`, `ZQ`, `C-c C-k` | Cancel (even after `:w`) |
| `:w` | Save the draft and keep editing |

Submitting an empty buffer cancels. Drafts are saved to disk as you type,
so nothing is lost if Emacs crashes. Code a comment or question is about is
shown above the text and isn't part of it.

## Configuration

Set these in `config.el` (after godel loads) or with `M-x customize-group
RET godel`. The new-task defaults can also be set per repo in
`.dir-locals.el`; they apply when you start the task from a buffer in that
repo (not from the board).

| Option | Default | What it does |
|---|---|---|
| `godel-default-model` | `"notdiamond[1m]"` | Model for new tasks (a `value` from the model list) |
| `godel-default-effort` | `"medium"` | Effort for new tasks and reviewers, on any model (Not Diamond included) |
| `godel-default-permission-mode` | `nil` | Permission mode for new tasks; `nil` is Claude's own default |
| `godel-reviewer-model` | `"notdiamond[1m]"` | Model for `v r` and `v w` |
| `godel-reviewer-max-diff` | `200000` | Largest diff (characters) put in a reviewer's prompt |
| `godel-reviewer-ask-author` | `t` | Whether a reviewer first asks the author for a briefing (one extra turn of the author's model; `nil` to skip) |
| `godel-reviewer-briefing-timeout` | `300` | Seconds to wait for the author's briefing before reviewing without it |
| `godel-reviewer-prepared-max-age` | `600` | Seconds a walkthrough's briefing is kept when you'd moved on before it arrived |
| `godel-notify-states` | needs-attention, needs-review, model-mismatch | States that trigger a desktop notification |
| `godel-board-workspace` | `"Godel"` | Name of the board's workspace |
| `godel-sidebar-width` | `40` | Sidebar width in columns |
| `godel-sidebar-auto-open` | `t` | Open the sidebar in task workspaces |
| `godel-ship-remote` | `"origin"` | Remote that task branches are pushed to |
| `godel-ship-check-interval` | `300` | Seconds between PR state checks (`nil`: only on the board) |
| `godel-gh-program` | `"gh"` | The GitHub CLI |
| `godel-worktree-root` | `~/.godel/worktrees/` | Where task worktrees go |
| `godel-directory` | `~/.godel/` | godel's own files (caches, repo list) |
| `godel-claude-program` | `"notdiamond-claude"` | Program that starts sessions |
| `godel-claude-binary` | `~/.local/bin/claude` | The real Claude binary (only its version is read) |
| `godel-notdiamond-program` | `notdiamond` | The notdiamond CLI (proxy status and start) |
| `godel-proxy-address` | `nil` | `(HOST . PORT)` of the notdiamond proxy; `nil` reads it from its config |
| `godel-agent-start-timeout` | `60` | Seconds to wait for a session to start |
| `godel-import-exclude-regexp` | Paradev worktrees | Worktrees never offered for import |

**Colours** come from your theme. To change one, set its face with
`custom-set-faces!`: `godel-state-<state>` (e.g. `godel-state-working`),
`godel-comment`, `godel-comment-bar`, `godel-comment-meta`,
`godel-reviewer-focus` (the walkthrough's highlight), and
`godel-compose-waiting` (the drafting status).

## Troubleshooting

**A task shows "model mismatch".** Its session started on a different model
than the task's and was stopped before your prompt was sent. The row says
what it started on. `R` restarts it (and lets you pick again). If it keeps
happening with fixed models, `SPC G M` re-checks the model pin; a warning
means the notdiamond wrapper changed and only Not Diamond works for now.

**A task is "stopped" after restarting Emacs.** Expected: sessions run inside
Emacs. Open the task (`RET`) to resume it.

**"error in process filter: … -32601, Method not found" in `*Messages*`,**
and the walkthrough's tools take a few seconds to connect. This comes from
claude-code-ide's Emacs tools server, not godel, and is harmless.

**A comment is "outdated".** The code it pointed at is gone. Delete it
(`SPC G x`) or leave it as a record.

**A ship step failed.** The board row names the step; the full output is
in `*godel-ship: <task>*`. Fix the cause (e.g. a rejected push) and ship
again.

**Something needs a closer look.** Each task's setup output is in
`*godel-setup: <task>*`, its ship commands in `*godel-ship: <task>*`, and
the Claude session in `*claude-code[<repo>/<slug>]*`.

## Limitations

- **Agents run on this machine, inside Emacs.** They stop when Emacs exits
  or the laptop sleeps. Running them on an always-on host (and checking in
  from a phone) is planned in `REMOTE-PLAN.md`.
- **Worktrees don't isolate everything:** tasks in the same repo share
  ports, databases and credentials.
- **Claude only,** through `notdiamond-claude`.
- **One tracked PR per task;** PRs opened outside godel aren't shown.
- **No board filter** yet.

## Key reference

**Anywhere (`SPC G`):**

| Key | Action |
|---|---|
| `SPC G a` | Board |
| `SPC G n` | New task |
| `SPC G o` | Toggle the sidebar |
| `SPC G i` | Import an existing worktree as a task |
| `SPC G M` | Refresh the model list (and re-check the model pin) |
| `SPC G c` | Comment on the line or selection (in a task's diff or files) |
| `SPC G x` | Delete the comment on the line (in a task's diff or files) |
| `SPC G v` | Review menu (in a task's diff or files) |

**Board and sidebar:** `RET` open · `TAB` expand · `n` new · `m` message ·
`R` restart · `x` tear down · `d` diff · `v` review menu · `P p` ship ·
`P d` ship as draft · `gp` open PR · `i` import · `gr` refresh · `q` quit.

**Review menu:** `d` diff · `l` list · `h` history · `s` send · `a` ask ·
`r`/`R` review · `w`/`W` walkthrough · `P`/`D` ship · `o` open PR.

**Task diff:** `C` comment · `gn`/`gp` next/previous comment.

**Review list:** `RET` visit · `C` edit · `x` delete/drop · `a` accept ·
`s` send · `gr` refresh.

**Compose buffer:** `:wq` submit · `:q!` cancel · `:w` save draft.
