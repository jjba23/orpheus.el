#!/usr/bin/env bash
# Record the orpheus demo GIF and its stills from a fixed library.
#
# SPDX-License-Identifier: MIT
# Copyright © Josep Bigorra <jjbigorra@gmail.com>
#
# The demo needs no player, daemon or network: Emacs draws the real interface
# over demo data, once per player theme and scene; ffmpeg encodes the frames
# and gifsicle joins the opening block to the rest.  Reproducible means the
# same checkout and the same tools produce the same frames; the only
# machine-dependent input is the font Emacs finds.
#
# EMACS overrides the Emacs binary, which is how a shell whose Emacs hangs on
# its first GTK frame can record with another one.

set -euo pipefail

root=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"

output=${1:-screenshots/orpheus-demo.gif}
emacs=${EMACS:-emacs}

for program in xvfb-run ffmpeg gifsicle "$emacs"; do
    if ! command -v "$program" >/dev/null 2>&1; then
        printf 'record-demo: %s is required\n' "$program" >&2
        exit 1
    fi
done

if [ ! -f screenshots/demo-cover.png ]; then
    printf 'record-demo: screenshots/demo-cover.png is missing\n' >&2
    exit 1
fi

frames=$(mktemp -d)
trap 'rm -rf "$frames"' EXIT

# The stills are named, so stale ones would survive a failed run.
rm -f screenshots/theme-*.png screenshots/layout-*.png \
      screenshots/visualizer-ember.png screenshots/visualizer-ice.png

ORPHEUS_DEMO_OUTPUT=$frames \
ORPHEUS_DEMO_STILLS=$root/screenshots \
    xvfb-run -a -s "-screen 0 1440x900x24" \
    "$emacs" -Q -L . -l scripts/record-demo.el \
          --eval '(run-at-time 0.5 nil (function orpheus-demo-run))'

frames_written=$(find "$frames" -name 'frame-*.png' | wc -l)
if [ "$frames_written" -eq 0 ]; then
    printf 'record-demo: Emacs wrote no frames\n' >&2
    exit 1
fi

# One palette for the whole recording keeps the UI colours flat; dithering
# would speckle them.  paletteuse drops the first four frames while it builds
# that palette, so the four-frame opening block is encoded on its own and the
# rest comes from the whole run with those frames dropped; gifsicle joins the
# two clean pieces.  Write beside the frames and move on success, so a failed
# encode never leaves a cut-off GIF in the repository.
tmp_gif=$frames/orpheus-demo.gif
tmp_rest=$frames/orpheus-rest.gif
tmp_full=$frames/orpheus-full.gif
ffmpeg -hide_banner -loglevel error -y -framerate 1 \
       -i "$frames/frame-%03d.png" \
       -vf "scale=760:-1:flags=lanczos,split [a][b];[a]palettegen=stats_mode=full [p];[b][p]paletteuse=dither=none" \
       -gifflags -transdiff -loop 0 -frames:v 4 "$tmp_gif"
ffmpeg -hide_banner -loglevel error -y -framerate 1 \
       -i "$frames/frame-%03d.png" \
       -vf "scale=760:-1:flags=lanczos,split [a][b];[a]palettegen=stats_mode=full [p];[b][p]paletteuse=dither=none" \
       -gifflags -transdiff -loop 0 "$tmp_rest"
gifsicle --loopcount=0 --delay=100 "$tmp_gif" "$tmp_rest" > "$tmp_full"
mv "$tmp_full" "$output"

printf 'record-demo: wrote %s (%s frames) and %s stills\n' \
       "$output" "$frames_written" \
       "$(find screenshots -name 'theme-*.png' -o -name 'layout-*.png' \
              -o -name 'visualizer-ember.png' -o -name 'visualizer-ice.png' | wc -l)"
