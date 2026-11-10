;;; orpheus-transient.el --- The orpheus command menu -*- lexical-binding: t -*-

;;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; A transient groups the commands a player needs by what they do: transport,
;;; the four modes, the queue, playlists, the library and the views.  It shows
;;; each binding next to its description, so the menu is the discoverable form
;;; of the keymap rather than a second list of keys that can drift from it.
;;; Every entry is an ordinary command, so a click, a key and `M-x' all reach
;;; the same function.

;;; Code:

(require 'transient)

(declare-function orpheus-equalizer-menu "orpheus-equalizer" ())
(declare-function orpheus-equalizer-editor "orpheus-equalizer" ())
(declare-function orpheus-equalizer-set-width "orpheus-equalizer" (width))
(declare-function orpheus-equalizer-set-max-gain "orpheus-equalizer" (gain))
(declare-function orpheus-equalizer-invert "orpheus-equalizer" ())
(declare-function orpheus-equalizer-tilt "orpheus-equalizer" ())
(declare-function orpheus-equalizer-flatten "orpheus-equalizer" ())
(declare-function orpheus-lyrics-open "orpheus-lyrics" ())
(declare-function orpheus-tutorial "orpheus-ui" ())
(declare-function orpheus-cycle-theme "orpheus-ui" ())
(declare-function orpheus-switch-backend "orpheus" (&optional backend))
(declare-function orpheus-ui-cycle-layout "orpheus-ui" ())
(declare-function orpheus-vis-cycle-theme "orpheus-vis" ())

;;; ###autoload
(transient-define-prefix orpheus-menu ()
  "The orpheus menu: transport, modes, queue, playlists and views."
  [["Transport"
    ("SPC" "Play or pause" orpheus-play-pause)
    ("s" "Stop" orpheus-stop)
    (">" "Next track" orpheus-next)
    ("<" "Previous track" orpheus-previous)
    ("f" "Seek forward" orpheus-seek-forward)
    ("b" "Seek back" orpheus-seek-backward)
    ("+" "Volume up" orpheus-volume-up)
    ("-" "Volume down" orpheus-volume-down)
    ("i" "Track info in the echo area" orpheus-show-info)
    ("o" "Open in another program" orpheus-open-externally)]
   ["Modes"
    ("r" "Cycle repeat: off, queue, track" orpheus-cycle-repeat)
    ("z" "Repeat" orpheus-toggle-repeat)
    ("x" "Random" orpheus-toggle-random)
    ("c" "Consume" orpheus-toggle-consume)
    ("v" "Single" orpheus-toggle-single)]
   ["Queue"
    ("a" "Queue this row or the marked rows" orpheus-ui-add)
    ("A" "Queue everything on this tab" orpheus-ui-add-all)
    ("d" "Delete the row or cancel the transfer" orpheus-ui-delete)
    ("D" "Clear the queue" orpheus-ui-clear)
    ("C" "Jump to the playing row" orpheus-ui-jump-to-current)]]
  [["Playlists"
    ("N" "New playlist" orpheus-ui-new-playlist)
    ("W" "Save the queue as a playlist" orpheus-ui-save-playlist)
    ("e" "Edit the playlist at point" orpheus-ui-edit-playlist)]
   ["Library"
    ("/" "Search" orpheus-ui-search)
    ("M-u" "Update the library" orpheus-update-database)
    ("M-U" "Rescan the library" orpheus-rescan-database)]
   ["View"
    ("TAB" "Next tab" orpheus-ui-next-tab)
    ("g" "Refresh" orpheus-refresh)
    ("V" "Visualizer" orpheus-visualizer)
    ("M-t" "Next player theme" orpheus-cycle-theme)
    ("M-l" "Next layout" orpheus-ui-cycle-layout)
    ("M-c" "Next visualizer theme" orpheus-vis-cycle-theme)
    ("E" "Equalizer presets" orpheus-equalizer-menu)
    ("M-E" "Edit the equalizer bands" orpheus-equalizer-editor)
    ("L" "Lyrics" orpheus-lyrics-open)
    ("B" "Switch to another backend" orpheus-switch-backend)
    ("H" "Help, tutorial and every key" orpheus-help)
    ("t" "Tutorial" orpheus-tutorial)
    ("q" "Bury the player" quit-window)]])

(provide 'orpheus-transient)

;;; orpheus-transient.el ends here
