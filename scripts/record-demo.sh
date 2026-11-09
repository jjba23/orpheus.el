#!/usr/bin/env bash
# Record the orpheus demo GIF from a fixed library.
#
# SPDX-License-Identifier: MIT
# Copyright © Josep Bigorra <jjbigorra@gmail.com>
#
# The demo needs no player, daemon or network: Emacs draws the real interface
# over demo data, once per theme, and ffmpeg assembles the frames.  Reproducible
# means the same checkout and the same tools produce the same frames; the only
# machine-dependent input is the font Emacs finds.

set -euo pipefail

root=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"

output=${1:-screenshots/orpheus-demo.gif}

for program in emacs xvfb-run ffmpeg; do
    if ! command -v "$program" >/dev/null 2>&1; then
        printf 'record-demo: %s is required\n' "$program" >&2
        exit 1
    fi
done

frames=$(mktemp -d)
trap 'rm -rf "$frames"' EXIT

ORPHEUS_DEMO_OUTPUT=$frames \
    xvfb-run -a -s "-screen 0 1200x800x24" \
    emacs -Q -L . -l scripts/record-demo.el \
          --eval '(run-at-time 0.5 nil (function orpheus-demo-run))'

frames_written=$(find "$frames" -name 'frame-*.png' | wc -l)
if [ "$frames_written" -eq 0 ]; then
    printf 'record-demo: Emacs wrote no frames\n' >&2
    exit 1
fi

ffmpeg -hide_banner -loglevel error -y -framerate 1 \
       -i "$frames/frame-%03d.png" \
       -vf "scale=672:-1:flags=lanczos,split [a][b];[a]palettegen=stats_mode=diff [p];[b][p]paletteuse=dither=bayer:bayer_scale=3" \
       -loop 0 "$output"

printf 'record-demo: wrote %s (%s frames)\n' "$output" "$frames_written"
