# godel remote: agents on an always-on host

Status (2026-11-17): planned, not started. This replaces the old Phase 3
("durability: a local tmux backend") in `PLAN.md`. Nothing here is built.
The open questions at the end need answers before the spike can start.

## Goal

Make godel depend less on the laptop. Today every agent is a process inside
Emacs on the laptop: it stops when the laptop sleeps, when Emacs restarts,
or when the laptop goes in a bag. Two situations motivate this:

- **On the go:** close the laptop and have the agents keep working.
- **Mobile:** see what needs attention, read an agent's reply and answer it
  from a phone.

A local tmux backend (the old Phase 3) only covers Emacs restarts: the
agents would still stop when the laptop sleeps. So the agents have to live
on a machine that is always on, and the laptop and the phone become clients
of it.

## Design

```
 cloud host (always on, on your Tailscale network)
 ┌────────────────────────────────────────────────────────┐
 │ headless Emacs daemon running godel ("the host")       │
 │   task store · hooks · worktrees · ship · reviewer     │
 │   event log ──────────────┐                            │
 │ tmux -L godel             │                            │
 │   task-a: notdiamond-claude ... (keeps running)        │
 │   task-b: ...             │                            │
 │ repos + worktrees, gh/git/claude/notdiamond auth       │
 └───────────▲───────────────┼─────────────▲──────────────┘
             │ RPC + ssh     │ events      │ ssh / push
   laptop: Doom (client)  ◄──┘          phone: Termius + ntfy
   board, sidebar, diffs (TRAMP),       attach to a task's tmux,
   terminals = ssh + tmux attach        get notified, reply
```

### The host

- **A headless Emacs daemon running godel.** Almost all of godel is elisp
  that doesn't need a display: hooks, the task store, worktrees, setup
  scripts, ship, the independent reviewer, PR checks. Running the same code
  on the host reuses it as is, without a second server written in another
  language. `godel-api.el` (built in Phase 1 for this kind of frontend)
  becomes the interface clients call.
- **A tmux backend.** Each task's Claude runs in its own tmux session on a
  godel-only tmux server (`tmux -L godel`), so it doesn't depend on any
  client being attached. The agent backend already has a small protocol
  (`godel-agent-start`, `-send`, `-stop`, `-buffer`, `-live-p`); tmux is a
  second implementation of it. Sending a prompt becomes `tmux load-buffer` +
  `paste-buffer -p` (bracketed paste) and Enter; reading the screen becomes
  `tmux capture-pane`.
- **Hooks stay local to the host.** `bin/godel-hook` calls the host's
  `emacsclient`, as today. If the daemon is down, the hook appends the event
  to a per-task log, and the daemon replays it on start, so no event is
  lost.
- **Credentials live on the host:** git and `gh` auth, Claude's login, the
  notdiamond config and proxy, and the repos themselves.
- **An event log.** Every `godel-event-functions` call is also written to a
  log (one line per event), which clients follow.

### The laptop client

- **Board and sidebar** draw from the host's state, read with
  `godel-api-tasks` over `emacsclient` (Tailscale, TCP server with an auth
  file, or `ssh host emacsclient`). They return plain Lisp data.
- **Live updates** come from following the host's event log over ssh
  (`ssh host tail -F …`), the way hooks drive redraws today.
- **Actions** (new task, message, restart, ship, review, teardown) are
  `godel-api-*` calls to the host.
- **Task terminals:** ghostel runs `ssh host -t tmux -L godel attach -t
  <task>`. Closing the laptop detaches; the agent keeps going.
- **Diffs, comments, review:** magit over TRAMP on the host's worktree.
  Comments are stored on the host, so they're the same from any client.
- **Things that must appear in "your" Emacs** (the walkthrough's
  `godel_show_location`, "draft ready", compose buffers) are sent as events
  to the attached client, which acts on them.

### The phone

- **Read and reply:** an ssh app (Termius, Blink) attaches to the task's
  tmux session.
- **Notifications:** the host sends a push through ntfy.sh on
  needs-attention and needs-review (one more function on
  `godel-event-functions`).
- **Later:** a small web page on the tailnet (board, task log, rendered
  diff, reply box). It's the only option where reading diffs on a phone is
  pleasant, and the most work.

### Local mode stays

Each repo is either local (as today) or on a host. Existing tasks and
workflows keep working unchanged.

## Alternatives considered

- **Run all of Doom on the host** in tmux and use it from a terminal
  (mosh/ssh). Almost no godel changes, but no GUI Emacs, and terminal Emacs
  on a phone is painful.
- **A separate server** (Go or Python) instead of headless Emacs: cleaner
  process model, but godel's state machine, worktree handling, ship and
  reviewer would be written twice.
- **Slack** as the mobile frontend (one thread per task, a Socket Mode bot
  calling `godel-api-*`): no app to build, but code and diffs end up in
  Slack.
- **Claude Code's own Remote Control or cloud sessions:** worth checking
  whether they work through `notdiamond-claude`; they wouldn't give godel's
  board, review loop or ship.
- **A local tmux backend only** (the old Phase 3): survives Emacs restarts,
  not a sleeping laptop.

## Spike (before building)

About a day, no changes to godel, findings in a write-up like `SPIKE.md`:

1. **claude-code-ide in a headless Emacs.** Its diff integration opens
   ediff and waits for the user; with no display that could hang a session.
   Find what has to be turned off on the host, and whether the Emacs tools
   server (godel's walkthrough tools) works there.
2. **magit over TRAMP** on a real task diff over Tailscale: how slow are
   `d`, comments and the review list?
3. **ghostel → ssh → tmux:** keys, colours, scrollback, and reconnecting
   after the laptop sleeps (plain ssh vs mosh).
4. **`notdiamond-claude` on the host:** the proxy, the model pin and the
   proxy guard, and its login.
5. **Event latency:** a hook on the host to a board redraw on the laptop.

## Build order (after the spike)

1. **Host daemon + tmux backend**, still used from a terminal on the host:
   agents survive client disconnects; hook replay.
2. **Laptop client:** board and sidebar from the host, actions over RPC,
   terminals over ssh + tmux, diffs over TRAMP, forwarded UI events.
3. **Phone:** ntfy push and ssh attach; the web page later if needed.

## Open questions

1. **Which machine?** A cloud VM (Hetzner, EC2, GCP), a company dev box, or
   one you already have. It must be always on and reach GitHub and Not
   Diamond's services.
2. **Company code off the laptop.** Can Not Diamond repos be cloned on that
   machine, with your `gh`, Claude and notdiamond credentials on it? This may
   need a check with whoever owns security.
3. **Tailscale** for laptop and phone access, so nothing is exposed
   publicly?
4. **Order:** spike, then host, then laptop client, then phone?

## Notes carried over from PLAN.md

- **Missed events today.** If Emacs isn't running, `emacsclient` fails and
  the hook event is lost (`godel-hook` always exits 0). Sessions run inside
  Emacs, so they end with it; `godel-mode` marks such tasks stopped and
  opening a task resumes its session. The host's hook log replaces this.
- **Rebuilding state from transcripts** only matters once sessions outlive
  the Emacs that started them, i.e. with the host.
- **Performance** doesn't need tmux: measured in Phase 0, 8 hidden terminals
  cost Emacs about 10% CPU (`SPIKE.md`). tmux here is for the host, not for
  speed.
