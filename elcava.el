;;; elcava.el --- Audio spectrum visualizer for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 emacs-os

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: emacs-os
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Audio spectrum visualizer for Emacs, inspired by cava.
;; Captures system audio via PipeWire's parec, computes FFT in pure Elisp,
;; and renders Unicode bar charts in a buffer.  Linux/PipeWire only.
;;
;; Usage: M-x elcava

;;; Code:

(require 'cl-lib)

;;;; Customization

(defgroup elcava nil
  "Audio spectrum visualizer."
  :group 'multimedia
  :prefix "elcava-")

(defcustom elcava-bars 24
  "Number of frequency bars."
  :type 'integer)

(defcustom elcava-framerate 60
  "Target frames per second."
  :type 'integer)

(defcustom elcava-fft-size 512
  "FFT window size (must be power of 2)."
  :type 'integer)

(defcustom elcava-sample-rate 44100
  "Audio sample rate in Hz."
  :type 'integer)

(defcustom elcava-low-cutoff 50
  "Low frequency cutoff in Hz."
  :type 'integer)

(defcustom elcava-high-cutoff 10000
  "High frequency cutoff in Hz."
  :type 'integer)

(defcustom elcava-noise-reduction 0.77
  "Smoothing factor (0.0=fast/noisy, 1.0=slow/smooth)."
  :type 'float)

(defcustom elcava-gravity 1.8
  "Gravity factor for bar falloff speed."
  :type 'float)

(defcustom elcava-parec-device "@DEFAULT_MONITOR@"
  "PipeWire/PulseAudio monitor device for parec."
  :type 'string)

(defcustom elcava-style 'spectrum
  "Visualization style.
Available: `spectrum', `spectrum-reverse', `spectrum-notes',
`spectrum-freq', `waveform'."
  :type '(choice (const :tag "Spectrum bars" spectrum)
                 (const :tag "Spectrum (reversed)" spectrum-reverse)
                 (const :tag "Spectrum with note labels" spectrum-notes)
                 (const :tag "Spectrum with frequency labels" spectrum-freq)
                 (const :tag "Waveform" waveform)))

;;;; Internal state

(defvar elcava--process nil "The parec async subprocess.")
(defvar elcava--timer nil "Render timer.")
(defvar elcava--pcm-chunks nil "List of unibyte PCM chunks (newest first).")
(defvar elcava--pcm-total 0 "Total bytes across all PCM chunks.")

;; FFT precomputed tables
(defvar elcava--log2n nil "Log base 2 of FFT size.")
(defvar elcava--hann nil "Hann window coefficients (vector).")
(defvar elcava--twiddle-re nil "Twiddle factor real parts (vector, length N/2).")
(defvar elcava--twiddle-im nil "Twiddle factor imaginary parts (vector, length N/2).")
(defvar elcava--bit-reverse nil "Bit-reversal permutation table (vector).")

;; FFT working buffers
(defvar elcava--fft-re nil "FFT real part working buffer.")
(defvar elcava--fft-im nil "FFT imaginary part working buffer.")

;; Frequency map
(defvar elcava--bin-lo nil "Lower FFT bin index per bar.")
(defvar elcava--bin-hi nil "Upper FFT bin index per bar.")
(defvar elcava--eq nil "EQ normalization factor per bar.")

;; Smoothing state
(defvar elcava--bars nil "Current smoothed bar values (vector of floats 0.0-1.0).")
(defvar elcava--prev nil "Previous frame post-gravity values.")
(defvar elcava--peak nil "Peak values per bar for gravity.")
(defvar elcava--fall nil "Fall counters per bar for gravity.")
(defvar elcava--mem nil "Exponential smoothing memory per bar.")
(defvar elcava--raw nil "Preallocated vector for raw bar magnitudes.")
(defvar elcava--sens 1.0 "Auto-sensitivity multiplier.")
(defvar elcava--sens-init t "Whether sensitivity is still in initial ramp-up.")

;; Rendering
(defvar elcava--colors nil "Vector of hex color strings per bar.")
(defvar elcava--char-cache nil "2D cache: [bar][level] → propertized string.")

;; Style cycling
(defvar elcava--styles '(spectrum spectrum-reverse spectrum-notes spectrum-freq waveform)
  "Available visualization styles for cycling.")
(defvar elcava--cut-freq nil "Cutoff frequency boundaries (vector, length bars+1).")
(defvar elcava--note-names ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"]
  "Musical note names by chromatic index.")

;;;; Color gradient

(defun elcava--hsv-to-hex (hue)
  "Convert HUE (0-360) to hex RGB string with full saturation and value."
  (let* ((h6 (/ (mod hue 360.0) 60.0))
         (i (floor h6))
         (f (- h6 i))
         (r 0.0) (g 0.0) (b 0.0))
    (pcase i
      (0 (setq r 1.0 g f     b 0.0))
      (1 (setq r (- 1.0 f) g 1.0 b 0.0))
      (2 (setq r 0.0 g 1.0 b f))
      (3 (setq r 0.0 g (- 1.0 f) b 1.0))
      (4 (setq r f     g 0.0 b 1.0))
      (_ (setq r 1.0 g 0.0 b (- 1.0 f))))
    (format "#%02x%02x%02x"
            (round (* r 255)) (round (* g 255)) (round (* b 255)))))

(defun elcava--init-colors ()
  "Precompute per-bar gradient colors (blue -> green -> red)."
  (let ((colors (make-vector elcava-bars "#ffffff")))
    (dotimes (i elcava-bars)
      (let* ((frac (/ (float i) (max 1 (1- elcava-bars))))
             (hue (* (- 1.0 frac) 240.0)))
        (aset colors i (elcava--hsv-to-hex hue))))
    (setq elcava--colors colors)))

(defun elcava--init-char-cache ()
  "Precompute propertized block characters per bar per fill level."
  (let* ((blocks " ▁▂▃▄▅▆▇█")
         (nbars elcava-bars)
         (cache (make-vector nbars nil)))
    (dotimes (b nbars)
      (let ((bar-cache (make-vector 9 nil))
            (color (aref elcava--colors b)))
        (dotimes (level 9)
          (let ((ch (char-to-string (aref blocks level))))
            (aset bar-cache level
                  (if (> level 0)
                      (propertize ch 'face `(:foreground ,color))
                    ch))))
        (aset cache b bar-cache)))
    (setq elcava--char-cache cache)))

;;;; X-axis label helpers

(defun elcava--bar-center-freq (b)
  "Return center frequency of bar B."
  (* 0.5 (+ (aref elcava--cut-freq b) (aref elcava--cut-freq (1+ b)))))

(defun elcava--freq-to-note (freq)
  "Return musical note name for FREQ Hz (e.g. \"A4\")."
  (let* ((midi (round (+ 69.0 (* 12.0 (log (/ freq 440.0) 2)))))
         (name (aref elcava--note-names (mod midi 12)))
         (octave (- (/ midi 12) 1)))
    (format "%s%d" name octave)))

(defun elcava--format-freq (freq)
  "Format FREQ Hz as short label (e.g. \"1k\", \"50\")."
  (if (>= freq 1000)
      (format "%dk" (round (/ freq 1000.0)))
    (format "%d" (round freq))))

(defun elcava--make-label-row (nbars reversed)
  "Build x-axis label string for NBARS bars.
REVERSED non-nil flips bar order.  Uses `elcava-style' to choose
note vs frequency labels."
  (let* ((row-width (max 1 (1- (* nbars 2))))
         (label-str (make-string row-width ?\s))
         (next-free 0))
    (dotimes (idx nbars)
      (let* ((b (if reversed (- nbars 1 idx) idx))
             (pos (* idx 2))
             (freq (elcava--bar-center-freq b))
             (label (pcase elcava-style
                      ('spectrum-notes (elcava--freq-to-note freq))
                      ('spectrum-freq  (elcava--format-freq freq))))
             (len (length label)))
        (when (and label (>= pos next-free) (<= (+ pos len) row-width))
          (dotimes (i len)
            (aset label-str (+ pos i) (aref label i)))
          (setq next-free (+ pos len 1)))))
    label-str))

;;;; FFT tables

(defun elcava--init-tables ()
  "Precompute Hann window, twiddle factors, and bit-reversal permutation."
  (let* ((n elcava-fft-size)
         (log2n (truncate (log n 2)))
         (hann (make-vector n 0.0))
         (tw-re (make-vector (/ n 2) 0.0))
         (tw-im (make-vector (/ n 2) 0.0))
         (br (make-vector n 0)))
    ;; Hann window
    (dotimes (i n)
      (aset hann i (* 0.5 (- 1.0 (cos (/ (* 2.0 float-pi i) (1- n)))))))
    ;; Twiddle factors: W_N^k = exp(-2*pi*i*k/N)
    (dotimes (k (/ n 2))
      (let ((angle (/ (* -2.0 float-pi k) n)))
        (aset tw-re k (cos angle))
        (aset tw-im k (sin angle))))
    ;; Bit-reversal permutation
    (dotimes (i n)
      (let ((rev 0) (val i))
        (dotimes (_ log2n)
          (setq rev (logior (ash rev 1) (logand val 1))
                val (ash val -1)))
        (aset br i rev)))
    (setq elcava--log2n log2n
          elcava--hann hann
          elcava--twiddle-re tw-re
          elcava--twiddle-im tw-im
          elcava--bit-reverse br
          elcava--fft-re (make-vector n 0.0)
          elcava--fft-im (make-vector n 0.0))))

;;;; Frequency mapping

(defun elcava--init-freq-map ()
  "Compute log-spaced frequency-to-FFT-bin mapping and EQ factors.
Port of cavacore.c frequency mapping algorithm."
  (let* ((bars elcava-bars)
         (n elcava-fft-size)
         (rate (float elcava-sample-rate))
         (lo-cut (float elcava-low-cutoff))
         (hi-cut (float elcava-high-cutoff))
         (half (/ n 2))
         (freq-const (/ (log (/ lo-cut hi-cut) 10)
                        (- (/ 1.0 (+ bars 1)) 1.0)))
         (cut-freq (make-vector (1+ bars) 0.0))
         (lower-bin (make-vector (1+ bars) 0))
         (bin-lo (make-vector bars 0))
         (bin-hi (make-vector bars 0))
         (eq-vec (make-vector bars 0.0))
         (min-bw (/ rate n)))
    ;; Compute log-spaced cutoff frequencies
    (dotimes (i (1+ bars))
      (let* ((coeff (+ (- freq-const)
                       (* (/ (float (1+ i)) (+ bars 1)) freq-const)))
             (freq (* hi-cut (expt 10.0 coeff))))
        (when (and (> i 0) (<= freq (aref cut-freq (1- i))))
          (setq freq (+ (aref cut-freq (1- i)) min-bw)))
        (aset cut-freq i freq)
        ;; Map to FFT bin
        (let ((bin (round (* (/ freq (/ rate 2.0)) half))))
          (aset lower-bin i (max 0 (min bin half))))))
    ;; Derive per-bar bin ranges, push up clumped bins
    (dotimes (i bars)
      (aset bin-lo i (aref lower-bin i))
      (let ((hi (max (aref lower-bin i)
                     (1- (aref lower-bin (1+ i))))))
        (aset bin-hi i hi))
      ;; Ensure no overlap with previous bar
      (when (and (> i 0) (<= (aref bin-lo i) (aref bin-hi (1- i))))
        (aset bin-lo i (1+ (aref bin-hi (1- i)))))
      ;; Ensure hi >= lo
      (when (< (aref bin-hi i) (aref bin-lo i))
        (aset bin-hi i (aref bin-lo i))))
    ;; EQ normalization (port of cavacore)
    (dotimes (i bars)
      (let* ((freq (aref cut-freq (1+ i)))
             (bin-count (max 1 (1+ (- (aref bin-hi i) (aref bin-lo i)))))
             (eq-val (/ (expt freq 0.85)
                        (* (expt 2.0 28.0) (log n 2) bin-count))))
        (aset eq-vec i eq-val)))
    (setq elcava--cut-freq cut-freq
          elcava--bin-lo bin-lo
          elcava--bin-hi bin-hi
          elcava--eq eq-vec)))

;;;; FFT — radix-2 Cooley-Tukey in-place

(defun elcava--fft ()
  "Compute in-place FFT on `elcava--fft-re' and `elcava--fft-im'."
  (let* ((n elcava-fft-size)
         (log2n elcava--log2n)
         (re elcava--fft-re)
         (im elcava--fft-im)
         (br elcava--bit-reverse)
         (tw-re elcava--twiddle-re)
         (tw-im elcava--twiddle-im))
    ;; In-place bit-reversal permutation (swap pairs where i < j)
    (dotimes (i n)
      (let ((j (aref br i)))
        (when (< i j)
          (let ((tmp (aref re i)))
            (aset re i (aref re j))
            (aset re j tmp))
          (let ((tmp (aref im i)))
            (aset im i (aref im j))
            (aset im j tmp)))))
    ;; Butterfly stages
    (dotimes (s log2n)
      (let* ((half (ash 1 s))
             (tw-stride (ash 1 (- log2n s 1)))
             (k 0))
        (while (< k n)
          (dotimes (j half)
            (let* ((tw-idx (* j tw-stride))
                   (wr (aref tw-re tw-idx))
                   (wi (aref tw-im tw-idx))
                   (i1 (+ k j))
                   (i2 (+ k j half))
                   (xr (aref re i2))
                   (xi (aref im i2))
                   (tr (- (* wr xr) (* wi xi)))
                   (ti (+ (* wr xi) (* wi xr))))
              (aset re i2 (- (aref re i1) tr))
              (aset im i2 (- (aref im i1) ti))
              (aset re i1 (+ (aref re i1) tr))
              (aset im i1 (+ (aref im i1) ti))))
          (setq k (+ k half half)))))))

;;;; Audio capture

(defun elcava--process-filter (_proc output)
  "Accumulate raw PCM bytes from parec OUTPUT.
O(1) per call — just pushes a chunk."
  (push output elcava--pcm-chunks)
  (cl-incf elcava--pcm-total (length output)))

(defun elcava--pcm-flatten ()
  "Coalesce PCM chunks into a single unibyte string.  Trims to 1s max."
  (when elcava--pcm-chunks
    (let* ((buf (apply #'concat (nreverse elcava--pcm-chunks)))
           (max-bytes (* elcava-sample-rate 2)))
      (when (> (length buf) max-bytes)
        (setq buf (substring buf (- (length buf) max-bytes))))
      (setq elcava--pcm-chunks (list buf)
            elcava--pcm-total (length buf))
      buf)))

(defun elcava--process-sentinel (_proc event)
  "Handle parec process EVENT."
  (when (string-match-p "\\(finished\\|exited\\|failed\\)" event)
    (elcava--cleanup)
    (message "elcava: parec process ended (%s)"
             (string-trim event))))

(defun elcava--start-capture ()
  "Start parec subprocess for audio capture."
  (setq elcava--pcm-chunks nil
        elcava--pcm-total 0)
  (setq elcava--process
        (make-process
         :name "elcava-parec"
         :command (list "parec"
                        (format "--device=%s" elcava-parec-device)
                        "--format=s16le"
                        (format "--rate=%d" elcava-sample-rate)
                        "--channels=1"
                        "--latency-msec=20"
                        "--raw")
         :connection-type 'pipe
         :coding 'binary
         :noquery t
         :filter #'elcava--process-filter
         :sentinel #'elcava--process-sentinel)))

;;;; Sample decoding

(defun elcava--drain-samples ()
  "Decode most recent FFT-window of S16LE bytes into windowed float samples.
Returns non-nil if enough data was available."
  (let* ((n elcava-fft-size)
         (bytes-needed (* n 2)))
    (when (>= elcava--pcm-total bytes-needed)
      (let* ((buf (elcava--pcm-flatten))
             (len (length buf))
             (offset (- len bytes-needed))
             (fft-re elcava--fft-re)
             (fft-im elcava--fft-im)
             (hann elcava--hann))
        (dotimes (i n)
          (let* ((byte-off (+ offset (* i 2)))
                 (lo (aref buf byte-off))
                 (hi (aref buf (1+ byte-off)))
                 (raw (logior lo (ash hi 8)))
                 (signed (if (>= raw 32768) (- raw 65536) raw)))
            (aset fft-re i (* (/ (float signed) 32768.0) (aref hann i)))
            (aset fft-im i 0.0)))
        ;; Keep only the last window
        (setq elcava--pcm-chunks (list (substring buf offset))
              elcava--pcm-total bytes-needed)
        t))))

;;;; Bar magnitude computation

(defun elcava--compute-bars ()
  "Compute raw bar magnitudes from FFT output into `elcava--raw'."
  (let* ((nbars elcava-bars)
         (re elcava--fft-re)
         (im elcava--fft-im)
         (bin-lo elcava--bin-lo)
         (bin-hi elcava--bin-hi)
         (eq-vec elcava--eq)
         (raw elcava--raw))
    (dotimes (b nbars)
      (let ((sum 0.0)
            (lo (aref bin-lo b))
            (hi (aref bin-hi b)))
        (while (<= lo hi)
          (let ((r (aref re lo))
                (i (aref im lo)))
            (setq sum (+ sum (sqrt (+ (* r r) (* i i))))))
          (setq lo (1+ lo)))
        (aset raw b (* sum (aref eq-vec b)))))))

;;;; Smoothing — gravity falloff + exponential smoothing + auto-sensitivity

(defun elcava--init-smoothing ()
  "Initialize smoothing state vectors."
  (let ((n elcava-bars))
    (setq elcava--bars (make-vector n 0.0)
          elcava--prev (make-vector n 0.0)
          elcava--peak (make-vector n 0.0)
          elcava--fall (make-vector n 0.0)
          elcava--mem  (make-vector n 0.0)
          elcava--raw  (make-vector n 0.0)
          elcava--sens 1.0
          elcava--sens-init t)))

(defun elcava--smooth-bars ()
  "Apply gravity falloff, exponential smoothing, and auto-sensitivity.
Reads from `elcava--raw', writes to `elcava--bars'.
Port of cavacore.c lines 399-452.
All frame-rate-dependent constants are scaled so behavior matches
cava's reference rate of 75 fps regardless of `elcava-framerate'."
  (let* ((nbars elcava-bars)
         (fps-ratio (/ 75.0 elcava-framerate))
         ;; Scale integral filter: same time-constant at any fps
         (nr (expt elcava-noise-reduction fps-ratio))
         (gravity-mod (max 1.0 (* (expt (/ 60.0 elcava-framerate) 2.5)
                                  (/ 1.54 elcava-noise-reduction))))
         (fall-rate (* 0.028 fps-ratio (/ elcava-gravity 1.8)))
         (overshoot nil)
         (silence t)
         (raw elcava--raw)
         (bars-out elcava--bars)
         (prev elcava--prev)
         (peak elcava--peak)
         (fall elcava--fall)
         (mem elcava--mem)
         (sens elcava--sens))
    (dotimes (i nbars)
      (let ((val (* (aref raw i) sens)))
        (when (/= val 0.0) (setq silence nil))
        ;; Gravity falloff
        (if (and (< val (aref prev i)) (> elcava-noise-reduction 0.1))
            (progn
              (setq val (* (aref peak i)
                           (- 1.0 (* (aref fall i) (aref fall i) gravity-mod))))
              (when (< val 0.0) (setq val 0.0))
              (aset fall i (+ (aref fall i) fall-rate)))
          (aset peak i val)
          (aset fall i 0.0))
        (aset prev i val)
        ;; Exponential smoothing (integral filter)
        (setq val (+ (* (aref mem i) nr) val))
        (aset mem i val)
        ;; Clamp and detect overshoot
        (when (> val 1.0)
          (setq overshoot t val 1.0))
        (aset bars-out i val)))
    ;; Auto-sensitivity — rates scaled by fps-ratio so ramp speed
    ;; is the same in wall-clock seconds at any framerate
    (cond
     (overshoot
      (setq elcava--sens (* elcava--sens (expt 0.98 fps-ratio))
            elcava--sens-init nil))
     ((not silence)
      (setq elcava--sens (* elcava--sens (expt 1.001 fps-ratio)))
      (when elcava--sens-init
        (setq elcava--sens (* elcava--sens (expt 1.1 fps-ratio))))))))

;;;; Rendering — spectrum modes

(defun elcava--render-spectrum ()
  "Draw spectrum bar visualization to the *elcava* buffer.
Handles normal, reversed, and label variants based on `elcava-style'."
  (let ((buf (get-buffer "*elcava*")))
    (when buf
      (let ((win (get-buffer-window buf t)))
        (when win
          (with-current-buffer buf
            (with-silent-modifications
              (let* ((total-rows (window-body-height win))
                     (has-labels (memq elcava-style '(spectrum-notes spectrum-freq)))
                     (rows (if has-labels (max 1 (1- total-rows)) total-rows))
                     (reversed (eq elcava-style 'spectrum-reverse))
                     (nbars (min elcava-bars (length elcava--bars)))
                     (bars elcava--bars)
                     (cache elcava--char-cache))
                (erase-buffer)
                ;; Bar rows
                (dotimes (r rows)
                  (let ((row-bottom (* (- rows r 1) 8)))
                    (dotimes (idx nbars)
                      (let* ((b (if reversed (- nbars 1 idx) idx))
                             (h (* (aref bars b) rows 8.0))
                             (fill (min 8 (max 0 (truncate (- h row-bottom))))))
                        (insert (aref (aref cache b) fill))
                        (when (< idx (1- nbars))
                          (insert " ")))))
                  (insert "\n"))
                ;; Label row
                (when has-labels
                  (insert (elcava--make-label-row nbars reversed)))
                (goto-char (point-min))))))))))

;;;; Waveform — RMS envelope computation

(defun elcava--compute-waveform ()
  "Compute RMS amplitude envelope from PCM data into `elcava--raw'.
Uses `elcava-bars' columns so rendering cost matches spectrum mode."
  (let* ((nbars elcava-bars)
         (pcm (elcava--pcm-flatten))
         (pcm-bytes (if pcm (length pcm) 0))
         (pcm-samples (/ pcm-bytes 2))
         (raw elcava--raw))
    (if (< pcm-samples (* nbars 4))
        (dotimes (i nbars) (aset raw i 0.0))
      (let* ((window (min pcm-samples (* nbars 80)))
             (spc (max 1 (/ window nbars)))
             (off (* (- pcm-samples window) 2)))
        (dotimes (b nbars)
          (let ((sum-sq 0.0)
                (base (+ off (* b spc 2))))
            (dotimes (s spc)
              (let* ((pos (min (- pcm-bytes 2) (+ base (* s 2))))
                     (lo (aref pcm pos))
                     (hi (aref pcm (1+ pos)))
                     (v (logior lo (ash hi 8))))
                (when (>= v 32768) (setq v (- v 65536)))
                (let ((f (/ (float v) 32768.0)))
                  (setq sum-sq (+ sum-sq (* f f))))))
            (aset raw b (sqrt (/ sum-sq spc)))))))))

;;;; Rendering — waveform mode

(defun elcava--render-waveform ()
  "Draw waveform as symmetric amplitude bars from center.
Uses the same `elcava--bars' (post-smoothing) as spectrum, so gravity
and auto-sensitivity give the same bouncy feel.  Sub-row precision on
the upper contour via block chars; full blocks on the lower mirror."
  (let ((buf (get-buffer "*elcava*")))
    (when buf
      (let ((win (get-buffer-window buf t)))
        (when win
          (with-current-buffer buf
            (with-silent-modifications
              (let* ((rows (window-body-height win))
                     (center (/ rows 2))
                     (half (float center))
                     (nbars (min elcava-bars (length elcava--bars)))
                     (bars elcava--bars)
                     (cache elcava--char-cache))
                (erase-buffer)
                (dotimes (r rows)
                  (let* ((dist (abs (- r center)))
                         (row-bottom (* dist 8))
                         (upper (< r center)))
                    (dotimes (b nbars)
                      (let* ((h (* (aref bars b) half 8.0))
                             (fill (min 8 (max 0 (truncate (- h row-bottom))))))
                        (insert (cond
                                 ((zerop fill) " ")
                                 (upper (aref (aref cache b) fill))
                                 (t     (aref (aref cache b) 8))))
                        (when (< b (1- nbars))
                          (insert " ")))))
                  (when (< r (1- rows))
                    (insert "\n")))
                (goto-char (point-min))))))))))

;;;; Frame orchestration

(defun elcava--render-frame ()
  "Timer callback: drain -> FFT -> bars -> smooth -> render."
  (condition-case err
      (when (and elcava--process (process-live-p elcava--process))
        (if (eq elcava-style 'waveform)
            ;; Waveform: RMS envelope -> same smoothing pipeline -> symmetric render
            (progn
              (elcava--compute-waveform)
              (elcava--smooth-bars)
              (elcava--render-waveform))
          ;; Spectrum modes: full FFT pipeline
          (if (elcava--drain-samples)
              (progn (elcava--fft)
                     (elcava--compute-bars))
            ;; No new data: zero raw bars so gravity decays
            (dotimes (i elcava-bars) (aset elcava--raw i 0.0)))
          (elcava--smooth-bars)
          (elcava--render-spectrum)))
    (error (message "elcava: %s" (error-message-string err)))))

;;;; Lifecycle

(defun elcava--cleanup ()
  "Tear down timer and process."
  (when elcava--timer
    (cancel-timer elcava--timer)
    (setq elcava--timer nil))
  (when (and elcava--process (process-live-p elcava--process))
    (delete-process elcava--process))
  (setq elcava--process nil))

(defun elcava-quit ()
  "Stop elcava and kill the buffer."
  (interactive)
  (elcava--cleanup)
  (when (get-buffer "*elcava*")
    (kill-buffer "*elcava*")))

(defun elcava-adjust-bars (delta)
  "Adjust bar count by DELTA and reinitialize."
  (setq elcava-bars (max 2 (min 64 (+ elcava-bars delta))))
  (elcava--init-freq-map)
  (elcava--init-smoothing)
  (elcava--init-colors)
  (elcava--init-char-cache)
  (message "elcava: %d bars" elcava-bars))

(defun elcava-more-bars ()
  "Increase bar count by 2."
  (interactive)
  (elcava-adjust-bars 2))

(defun elcava-fewer-bars ()
  "Decrease bar count by 2."
  (interactive)
  (elcava-adjust-bars -2))

(defun elcava-cycle-style ()
  "Cycle to the next visualization style."
  (interactive)
  (let* ((tail (cdr (memq elcava-style elcava--styles)))
         (next (or (car tail) (car elcava--styles))))
    (setq elcava-style next)
    (message "elcava: %s" elcava-style)))

;;;; Mode

(defvar elcava-mode-map
  (make-sparse-keymap)
  "Keymap for `elcava-mode'.")

;; Bindings outside defvar so they apply on re-eval too.
(define-key elcava-mode-map (kbd "q") #'elcava-quit)
(define-key elcava-mode-map (kbd "+") #'elcava-more-bars)
(define-key elcava-mode-map (kbd "=") #'elcava-more-bars)
(define-key elcava-mode-map (kbd "-") #'elcava-fewer-bars)
(define-key elcava-mode-map (kbd "m") #'elcava-cycle-style)

(define-derived-mode elcava-mode special-mode "Elcava"
  "Major mode for the elcava audio visualizer."
  :group 'elcava
  (setq buffer-undo-list t
        truncate-lines t
        cursor-type nil)
  (add-hook 'kill-buffer-hook #'elcava--cleanup nil t))

;;;; Entry point

;;;###autoload
(defun elcava ()
  "Start the audio spectrum visualizer.
Captures system audio via PipeWire/PulseAudio and renders a
real-time frequency bar chart using Unicode block characters."
  (interactive)
  (unless (executable-find "parec")
    (user-error "parec not found; install PipeWire or PulseAudio"))
  ;; Clean up any previous instance
  (elcava--cleanup)
  ;; Initialize all tables and state
  (elcava--init-tables)
  (elcava--init-freq-map)
  (elcava--init-smoothing)
  (elcava--init-colors)
  (elcava--init-char-cache)
  ;; Setup buffer
  (let ((buf (get-buffer-create "*elcava*")))
    (with-current-buffer buf
      (elcava-mode))
    (pop-to-buffer buf))
  ;; Start capture and render timer
  (elcava--start-capture)
  (setq elcava--timer
        (run-at-time 0 (/ 1.0 elcava-framerate) #'elcava--render-frame)))

(provide 'elcava)
;;; elcava.el ends here
