;;; Development and demo environment for orpheus.el.
;;; SPDX-License-Identifier: MIT
;;; Copyright © 2026 Josep Bigorra <jjbigorra@gmail.com>

;; Xvfb, ffmpeg, gifsicle and a Cairo-capable Emacs are what
;; scripts/record-demo.sh needs; DejaVu Sans Mono is the font the recorder asks
;; for.  Run:
;;   guix shell --pure -m manifest.scm -- bash scripts/record-demo.sh
(specifications->manifest
 '("bash"
   "coreutils"
   "emacs"
   "ffmpeg"
   "findutils"
   "font-dejavu"
   "gifsicle"
   "fontconfig"
   "xvfb-run"))
