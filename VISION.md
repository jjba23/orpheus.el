# Vision

orpheus is a music player for Emacs whose argument is structural: the interface
and the music source are separate, and a player built that way outlives the
source it was written for. MPD, a folder of files and a library on another
machine already appear in one buffer. This document says where that seam should
carry the project next, and what would have to change.

## Where orpheus is today

Twelve Emacs Lisp modules, roughly fifteen thousand lines, no dependencies
outside Emacs 29.1, MIT throughout. Three backends ship and are the reference
implementations: `orpheus-mpd.el`, a complete MPD client on
`make-network-process` with its own reply parser, `ACK` handling and the
`albumart`/`readpicture` binary payloads; `orpheus-local.el`, mpv over its JSON
IPC socket with the queue in Emacs; and `orpheus-ssh.el`, which browses a
remote tree with `ls -1p` and `find`, queues downloads with background
prefetch, and cuts a disc track on the host with `ffmpeg` or fetches only its
byte range with `dd` when the host has none.

What a user sees — tabs, the now-playing card, six themes, four layouts, the
equalizer, the lyric window, a spectrum visualizer drawn as SVG — is text,
faces and text properties in an ordinary buffer, so the terminal and the
graphical frame show the same player, and `B` swaps the backend without a
restart.

Five decisions constrain everything that follows.

- **The backend is a seam, not a subclass.** `orpheus-backend.el` is pure: the
  values (`orpheus-track`, `orpheus-status`, `orpheus-item`, `orpheus-lyrics`),
  the generics, and a registry keyed by symbol. A backend claims what it can
  serve through `orpheus-backend-capabilities`, so a thin one degrades by
  offering fewer tabs rather than by lying about them.
- **Nothing blocks.** No synchronous socket read and no
  `shell-command-to-string` sits in the path of the interface. One timer reads
  status; the list is redrawn only when something it shows changed.
- **The queue lives where it can be authoritative.** MPD owns its queue in the
  daemon and is gapless because of it. The daemonless backends keep the queue
  in Emacs, so the local backend plays one file at a time and the gap between
  two tracks is the time Emacs takes to load the next.
- **A disc image is many tracks.** A cue sheet — a `.cue` sidecar, a `CUESHEET`
  Vorbis comment or a FLAC `CUESHEET` block — becomes one row per track, and the
  range travels in the URI as `#start=…&end=…`, so a queue entry, a playlist and
  a cache key keep it without a second data structure.
- **Two worlds, one interface.** `display-graphic-p` is the only test that
  matters for images and SVG; everything else has an ASCII fallback.

The limits are in `docs/features.org`: no per-track rating, no command mode with
arguments, no pane splitting inside a tab, no mouse drag of queue rows, no
gapless playback on the local backend. The Windows named-pipe path is compiled
but no Windows machine runs in the gate, so it is unproven rather than
supported. The equalizer reaches only the backends that play through mpv.

## The vision

1. **A backends-per-year project, not a player with three backends.** Someone
   who has never read `orpheus-ssh.el` writes a fourth backend, tests it against
   a documented conformance harness, and has it merged. The measure is not the
   count: it is that the protocol is small enough to hold in one head.
2. **The queue is the player's, whichever backend is behind it.** Gapless
   playback, the next track preloaded, and a queue that survives a backend
   switch with its order and marks intact. MPD gets this today because the
   daemon owns the queue; the local and SSH backends should get it without the
   queue leaving Emacs.
3. **The player joins the desktop without joining a toolkit.** Media keys
   through MPRIS, playback hand-off to another machine's speakers, a library
   index two Emacs sessions can read. Optional, off by default, and no reason
   the package stops being pure Emacs Lisp on a terminal.
4. **Listening is reviewable without being a service.** Play counts, ratings,
   ReplayGain and an optional scrobble to ListenBrainz, kept in a file the user
   owns, so a recommendation can be argued with and a decade of history is not
   somebody else's database.
5. **An index, not a search that walks the tree.** One persistent index of tags,
   durations and cue sheets — maintained incrementally by the watcher the local
   backend already runs — shared by browsing, search and the SSH backend, which
   today re-reads a cue sheet per folder because it has nowhere to keep it.

## Features that are in scope and not built yet

- **Gapless playback on the local backend.** `orpheus-local.el` names the cost
  already. The hard part is that mpv plays one file at a time: either the queue
  moves into mpv's playlist — giving up Emacs's claim to be the authority — or a
  second mpv is preloaded and switched without a gap, which means two players,
  one silent, and a volume and position handoff that has to be exact.
- **Per-track rating.** Anticipated by the idle rows in the queue. The hard part
  is where the number lives: a rating in a FLAC tag is invisible over MPD, and
  one in the cache dies when the cache is pruned at `orpheus-cache-max-bytes`.
- **A command mode with arguments.** The keymap and the transient menu are
  there; what is missing is a read loop that takes an argument — a seek by
  minutes, a volume in decibels, a preset by name — without breaking the `RET`,
  `m` and `^` conventions a Dired user expects.
- **Pane splitting inside a tab, and mouse drag of queue rows.** The layout is a
  list of sections in one buffer: `tabs`, `path`, `art`, `now-playing`,
  `separator`, `list`, `footer`. A second list means two mark sets, two points
  and two fold states in one buffer, and a drag means moving rows over text
  properties without a toolkit and without fighting the mark semantics.
- **An equalizer for MPD.** `orpheus-backend-equalizer` answers `nil` for a
  backend whose audio leaves through another process. Reaching MPD means writing
  a filter chain into `mpd.conf` and asking the daemon to reprocess its output —
  a configuration change rather than a command.

## New features that would be worth having

- **A backend for a personal music server** (Subsonic-compatible, Navidrome,
  Jellyfin, Ampache): the seam's real test, since it means HTTP, JSON, a token
  and a queue that lives server-side. `docs/backends.org` ships no backend for a
  subscription service, because its sign-in belongs to an account; a self-hosted
  server has no such problem.
- **Scrobbling as a hook, not a feature.** One function on the track-change
  hook, off by default, ListenBrainz first because it takes a token rather than
  a password — and ReplayGain or EBU R128 handed to mpv as a gain filter, so a
  shuffled queue does not need the volume key for every track.
- **Smart playlists as data.** A playlist that is a list of clauses — artist,
  year, rating, play count, last played — resolved to an M3U8 on demand; the
  portable-playlist decision already makes the result a file, not an object.
- **An index keyed by content hash, not by path**, so a renamed file keeps its
  rating and its play count; and a **visualizer that needs no cava**, reading
  mpv's `--ao=pcm` or a filter fifo, so a machine that already runs the player
  needs no second binary.

## Contributions that would move the needle

1. **A backend conformance harness: a fake backend plus the tests every backend
   must pass.** Highest value per hour here. It turns "implement these generics"
   into "make these assertions green", and that is the difference between a
   documented protocol and a testable one.
2. **Gapless on the local backend, behind a flag.** A weekend of work with a
   real design question inside it, and the complaint MPD users make about every
   other backend.
3. **A fourth backend, in tree.** A Subsonic-compatible one proves both the seam
   and the harness, and gives the project a server-side queue that is not MPD's.
4. **The Windows gate.** One machine, one afternoon of failures, and a platform
   table stops saying "compiled but unproven".
5. **Rating and play-count storage that survives a rename.** Small, touches the
   cache's ownership rules, and unlocks the smart-playlist idea above.
6. **A scrobble hook with a ListenBrainz implementation and an opt-out test.**
   Self-contained, and the kind of feature that puts a package in someone's
   configuration.
7. **One page per generic, with one implementation and one that deliberately
   refuses.** `docs/backends.org` assumes the reader will read three backends.

## Compared to related projects

| Project | What it is | Where orpheus differs |
| --- | --- | --- |
| mpc.el | The MPD client built into Emacs, with a playlist buffer and a tag browser | MPD only; no local or remote-file backends, no cover art, equalizer or visualizer, nothing to switch when the daemon is absent |
| EMMS | The long-standing Emacs multimedia system, a player per source and a playlist buffer | Many sources, but a line-oriented interface with no backend protocol, no asynchronous guarantee, no visualizer or equalizer |
| Bongo | An Emacs media player with a tree browser and a media-column UI | Local files only, closer to a library editor than a remote-first player; no transfer queue, no registry |
| ncmpcpp, mpc | Terminal clients for MPD, with excellent keys and a rich UI | Deep MPD features and a real playlist editor, but no Emacs integration, no local backend, no browsing over SSH |
| mpv.el | A thin Emacs controller for mpv: one process, one file | The closest thing to the local backend, and deliberately smaller: no tabs, no library, no queue, no backends |
| Strawberry, Lollypop, Elisa | Desktop players with a scanned library database | Ratings, smart playlists, scrobbling, ReplayGain — the gaps named above; but GTK applications, not buffers, with no seam |
| cmus | A terminal player with its own library and a queue | Fast and keyboard-first, with a library index; no Emacs, no protocol, nothing that reaches another machine's files |

The desktop players are ahead on everything a database buys: incremental
scanning, ratings, play counts, smart playlists, ReplayGain and scrobbling. That
is not a coincidence — it is most of what this document's vision is made of, and
none of it requires orpheus to become a GTK application. The terminal players
are ahead on library-scale search without a daemon, which the local backend has
partly and the SSH backend has not.

Where orpheus is ahead, and should stay ahead: the seam. No other Emacs player
puts a daemon, a folder and another machine in one buffer under one keymap, and
no player of any kind treats a library behind a slow link as a first-class case
with prefetch, range fetches and a transfer tab reporting bytes and ETA. The
text-first interface is the second advantage: the same player works in a
terminal, in a frame and over SSH, which no GTK player can claim and no TUI
player can extend with a mouse drag or an inline image. Both are cheap to keep
and expensive to recover, so moving the queue into a daemon, or the interface
into a toolkit, would spend one of them.

## In the long run

- **The protocol becomes the product.** A documented, conformance-tested seam
  is what lets a player outlive its author's taste in backends, and the only
  part of this repository a second project could adopt.
- **A shared library index.** One index of tags, durations and cue sheets,
  written by the local backend and readable by the SSH backend and a second
  Emacs session, turns four searches into one lookup and makes ratings possible
  without a database server.
- **Listening history as a documented file format.** Play counts and ratings
  belong in a file another tool can read, not in a cache column; a small bet
  now, an expensive one to retrofit.
- **Reaching the machine you are not sitting at.** The SSH backend is the first
  step; hand-off between two machines running orpheus is the second, and it
  needs nothing more exotic than the protocol and the transfer queue.
- **Windows and macOS as tested platforms.** Both mean the external program does
  the platform-specific work; both need a machine in the gate first.

## Reading this document

Nothing here is scheduled. The in-scope items are the ones the code anticipates
and a contributor could start today; the vision statements would change the
shape of the repository and are worth arguing about before they are worth
building. The gaps in `docs/features.org` are still the honest answer to what
it does not do; this document is an argument about their order.

*This document describes intent, not commitments.*
