;;; html-table-colorizer.el --- Colorize HTML table rows based on content -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: html, tables, highlighting, colorize
;; URL: https://github.com/yourusername/html-table-colorizer

;;; Commentary:

;; This package provides functionality to colorize HTML table rows based on
;; textual content in cells.  Particularly useful for tables exported from
;; Org mode or other markup formats.
;;
;; To use:
;; 1. Define highlighting rules via `htc-highlight-rules'.
;; 2. Process HTML files with `htc-colorize-html-file'.
;; 3. For Org mode, add `htc-colorize-current-html-export' to your export hook.

;;; Code:

(require 'cl-lib)

;;;; Customization

(defgroup html-table-colorizer nil
  "Customization group for HTML table colorization."
  :group 'html
  :prefix "htc-")

(defcustom htc-highlight-rules
  `((th . (("Requirement" . "#5089be")))
    (td . (("TODO" . "#ffe9e7")
           ("DOING" . "#e7ffe9")
           ("DONE" . "#eeecff"))))
  "Alist of elements ('th or 'td) and associated keywords/colors for row highlighting."
  :type '(alist :key-type symbol :value-type alist)
  :group 'html-table-colorizer)

(defvar htc-available-themes
  '(("Solarized" . htc-setup-solarized-theme)
    ("Modern UI" . htc-setup-modern-theme)
    ("Mono Blue" . htc-setup-mono-blue-theme)
    ("Green-Blue" . htc-setup-green-blue-theme)
    ("Earth Tones" . htc-setup-earth-tones-theme)
    ("High Contrast" . htc-setup-high-contrast-theme)
    ("Purple-Pink" . htc-setup-purple-pink-theme)
    ("Mono Gray" . htc-setup-mono-gray-theme)
    ("Dark Mode" . htc-setup-dark-mode-theme)
    ("Autumn" . htc-setup-autumn-theme)
    ("Pastel" . htc-setup-pastel-theme)
    ("Emacs Default" . htc-setup-emacs-default-theme)
    ("Emacs Adwaita" . htc-setup-emacs-adwaita-theme)
    ("Emacs Tango" . htc-setup-emacs-tango-theme)
    ("Emacs Leuven" . htc-setup-emacs-leuven-theme))
  "Association list of available themes and their setup functions.")

(defun htc-select-theme ()
  "Interactively select and apply a theme from the available options."
  (interactive)
  (let* ((theme-names (mapcar #'car htc-available-themes))
         (selected-name (completing-read "Select theme: " theme-names nil t))
         (theme-func (cdr (assoc selected-name htc-available-themes))))
    (if theme-func
        (progn
          (funcall theme-func)
          (message "Applied '%s' theme" selected-name))
      (message "Theme '%s' not found" selected-name))))

(defun htc-select-and-preview-theme ()
  "Interactively select a theme with option to preview before applying."
  (interactive)
  (let* ((theme-names (mapcar #'car htc-available-themes))
         (preview-option "[Preview first]")
         (options (cons preview-option theme-names))
         (selected-option (completing-read "Select theme (or preview): " options nil t)))
    (cond
     ((string= selected-option preview-option)
      ;; Preview mode selected
      (let* ((preview-theme (completing-read "Select theme to preview: " theme-names nil t))
             (theme-func (cdr (assoc preview-theme htc-available-themes))))
        (if theme-func
            (htc-preview-theme theme-func)
          (message "Theme '%s' not found" preview-theme))))
     (t
      ;; Direct application selected
      (let ((theme-func (cdr (assoc selected-option htc-available-themes))))
        (if theme-func
            (progn
              (funcall theme-func)
              (message "Applied '%s' theme" selected-option))
          (message "Theme '%s' not found" selected-option)))))))

(defun htc-apply-random-theme ()
  "Apply a randomly selected theme from the available options."
  (interactive)
  (let* ((theme-funcs (mapcar #'cdr htc-available-themes))
         (selected-index (random (length theme-funcs)))
         (selected-func (nth selected-index theme-funcs))
         (selected-name (car (rassoc selected-func htc-available-themes))))
    (funcall selected-func)
    (message "Applied random theme: '%s'" selected-name)))

(defun htc-list-available-themes ()
  "Display a buffer with all available themes."
  (interactive)
  (with-output-to-temp-buffer "*Available HTML Table Themes*"
    (princ "Available HTML Table Colorizer Themes:\n\n")
    (dolist (theme htc-available-themes)
      (princ (format "• %s\n" (car theme))))
    (princ "\nUse M-x htc-select-theme to choose a theme\n")
    (princ "Use M-x htc-select-and-preview-theme to preview before applying\n")
    (princ "Use M-x htc-apply-random-theme to apply a random theme\n")))

;;;; Color Conversion Functions

(defun htc-hex-to-hsl (hex-color)
  "Convert HEX-COLOR to HSL colorspace (hue, saturation, lightness)."
  (let* ((r (/ (string-to-number (substring hex-color 1 3) 16) 255.0))
         (g (/ (string-to-number (substring hex-color 3 5) 16) 255.0))
         (b (/ (string-to-number (substring hex-color 5 7) 16) 255.0))
         (max-val (max r g b))
         (min-val (min r g b))
         (delta (- max-val min-val))
         (l (/ (+ max-val min-val) 2.0))
         (s (if (<= delta 0.0) 0.0
              (/ delta (- 1.0 (abs (- (* 2.0 l) 1.0))))))
         (h (cond
             ((<= delta 0.0) 0.0)
             ((= max-val r) (mod (/ (- g b) delta) 6.0))
             ((= max-val g) (+ (/ (- b r) delta) 2.0))
             (t (+ (/ (- r g) delta) 4.0)))))
    (list (* h 60.0) (* s 100.0) (* l 100.0))))

(defun htc-hsl-to-hex (h s l)
  "Convert HSL (hue, saturation, lightness) to hex color format."
  (let* ((h (/ (mod h 360.0) 60.0))
         (s (/ s 100.0))
         (l (/ l 100.0))
         (c (* (- 1.0 (abs (- (* 2.0 l) 1.0))) s))
         (x (* c (- 1.0 (abs (- (mod h 2.0) 1.0)))))
         (m (- l (/ c 2.0)))
         r g b)
    (cond
     ((and (>= h 0.0) (< h 1.0)) (setq r (+ c m) g (+ x m) b (+ 0.0 m)))
     ((and (>= h 1.0) (< h 2.0)) (setq r (+ x m) g (+ c m) b (+ 0.0 m)))
     ((and (>= h 2.0) (< h 3.0)) (setq r (+ 0.0 m) g (+ c m) b (+ x m)))
     ((and (>= h 3.0) (< h 4.0)) (setq r (+ 0.0 m) g (+ x m) b (+ c m)))
     ((and (>= h 4.0) (< h 5.0)) (setq r (+ x m) g (+ 0.0 m) b (+ c m)))
     ((and (>= h 5.0) (< h 6.0)) (setq r (+ c m) g (+ 0.0 m) b (+ x m))))
    (format "#%02x%02x%02x"
            (round (* r 255.0))
            (round (* g 255.0))
            (round (* b 255.0)))))

;;;; Color Generation Functions
(defun htc-generate-pastel-rules (base-pattern max-repeats)
  "Generate rules using soft pastel colors for a gentle, calming appearance."
  (let ((palette '("#f6f6ff" ;; Very light lavender
                   "#f8e1e7" ;; Soft pink
                   "#e7f0e9" ;; Pale mint
                   "#f2f1d7" ;; Light cream
                   "#e5e2f2" ;; Pale periwinkle
                   "#e9f0f2" ;; Light sky
                   "#f0e9e1" ;; Pale peach
                   "#e2f2e5")) ;; Soft mint
        (rules nil))
    (dotimes (i max-repeats)
      (let* ((repeats (- max-repeats i))
             (pattern (apply 'concat (make-list repeats base-pattern)))
             (color-index (mod (- max-repeats repeats) (length palette)))
             (color (nth color-index palette)))
        (push (cons pattern color) rules)))
    (nreverse rules)))

;;;; Emacs Default Theme Palette
;; Based on the default Emacs theme colors

(defun htc-generate-emacs-default-rules (base-pattern max-repeats)
  "Generate rules using the classic Emacs default theme colors."
  (let ((palette '("#ffffff" ;; White
                   "#f5f5f5" ;; Near white
                   "#ededed" ;; Default bg
                   "#e0e0e0" ;; Light gray
                   "#d1d1d1" ;; Medium light gray
                   "#c3c3c3" ;; Medium gray
                   "#b4b4b4" ;; Gray
                   "#a6a6a6")) ;; Dark gray
        (rules nil))
    (dotimes (i max-repeats)
      (let* ((repeats (- max-repeats i))
             (pattern (apply 'concat (make-list repeats base-pattern)))
             (color-index (mod (- max-repeats repeats) (length palette)))
             (color (nth color-index palette)))
        (push (cons pattern color) rules)))
    (nreverse rules)))

;;;; Emacs Themes - Additional Classic Emacs Themes

(defun htc-generate-emacs-adwaita-rules (base-pattern max-repeats)
  "Generate rules based on the Adwaita theme from Emacs."
  (let ((palette '("#ededed" ;; Background
                   "#e5e5e5" ;; Light gray
                   "#dddddd" ;; Medium light gray
                   "#d5d5d5" ;; Gray
                   "#cdcdcd" ;; Medium gray
                   "#c5c5c5" ;; Dark gray
                   "#bdbdbd" ;; Darker gray
                   "#b5b5b5")) ;; Darkest gray
        (rules nil))
    (dotimes (i max-repeats)
      (let* ((repeats (- max-repeats i))
             (pattern (apply 'concat (make-list repeats base-pattern)))
             (color-index (mod (- max-repeats repeats) (length palette)))
             (color (nth color-index palette)))
        (push (cons pattern color) rules)))
    (nreverse rules)))

(defun htc-generate-emacs-tango-rules (base-pattern max-repeats)
  "Generate rules based on the Tango theme from Emacs."
  (let ((palette '("#eeeeec" ;; Aluminum 1
                   "#e4e4e2" ;; Light aluminum
                   "#d3d7cf" ;; Aluminum 2
                   "#c8cec3" ;; Light gray-green
                   "#babdb6" ;; Aluminum 3
                   "#aeb5ac" ;; Medium gray
                   "#a2a9a1" ;; Gray-green
                   "#989d96")) ;; Dark gray-green
        (rules nil))
    (dotimes (i max-repeats)
      (let* ((repeats (- max-repeats i))
             (pattern (apply 'concat (make-list repeats base-pattern)))
             (color-index (mod (- max-repeats repeats) (length palette)))
             (color (nth color-index palette)))
        (push (cons pattern color) rules)))
    (nreverse rules)))

(defun htc-generate-emacs-leuven-rules (base-pattern max-repeats)
  "Generate rules based on the Leuven theme from Emacs."
  (let ((palette '("#ffffff" ;; White
                   "#f2f6fd" ;; Very light blue
                   "#e8eef5" ;; Light blue-gray
                   "#dde4ee" ;; Pale blue
                   "#d3dae7" ;; Light steel blue
                   "#c9d0e0" ;; Steel blue
                   "#bfc6d9" ;; Medium steel blue
                   "#b5bcd2")) ;; Dark steel blue
        (rules nil))
    (dotimes (i max-repeats)
      (let* ((repeats (- max-repeats i))
             (pattern (apply 'concat (make-list repeats base-pattern)))
             (color-index (mod (- max-repeats repeats) (length palette)))
             (color (nth color-index palette)))
        (push (cons pattern color) rules)))
    (nreverse rules)))

(defun htc-generate-gradient-color (base-color position)
  "Generate a color along a gradient based on POSITION.
BASE-COLOR is the starting hex color, POSITION is between 0.0-1.0."
  (let* ((r (string-to-number (substring base-color 1 3) 16))
         (g (string-to-number (substring base-color 3 5) 16))
         (b (string-to-number (substring base-color 5 7) 16))
         ;; Adjust colors (shifts toward white)
         (r-shift (+ r (* position (- 255 r))))
         (g-shift (+ g (* position (- 255 g))))
         (b-shift (+ b (* position (- 255 b)))))
    (format "#%02x%02x%02x" 
            (round r-shift) 
            (round g-shift) 
            (round b-shift))))

(defun htc-generate-hsl-gradient-color (base-color position)
  "Generate a color gradient using HSL for better perceptual linearity.
BASE-COLOR is the starting hex color, POSITION is 0.0-1.0."
  (let* ((hsl (htc-hex-to-hsl base-color))
         (h (nth 0 hsl))
         (s (nth 1 hsl))
         (l (nth 2 hsl))
         ;; Adjust lightness while preserving hue and saturation
         (l-target (+ l (* position (- 95.0 l)))))
    (htc-hsl-to-hex h s l-target)))

(defun htc-generate-complementary-gradient (base-color position max-positions)
  "Generate a color gradient that shifts slightly toward complementary colors.
BASE-COLOR is the starting hex color, POSITION and MAX-POSITIONS determine the gradient."
  (let* ((hsl (htc-hex-to-hsl base-color))
         (h (nth 0 hsl))
         (s (nth 1 hsl))
         (l (nth 2 hsl))
         ;; Shift hue slightly (up to 20 degrees) for variety
         (h-shift (* (mod position 2) 20.0))
         (new-h (mod (+ h h-shift) 360.0))
         ;; Adjust lightness based on position
         (l-factor (/ (float position) (float max-positions)))
         (new-l (+ l (* l-factor (- 90.0 l)))))
    (htc-hsl-to-hex new-h s new-l)))

;;;; Pattern Rule Generation

(defun htc-generate-pattern-rules (base-pattern max-repeats &optional base-color color-algorithm)
  "Generate rules for repeating BASE-PATTERN up to MAX-REPEATS times.
Optional BASE-COLOR to start gradient from (defaults to #b1b9d7).
COLOR-ALGORITHM can be 'basic, 'hsl, or 'complementary."
  (let ((color (or base-color "#b1b9d7"))
        (algo (or color-algorithm 'hsl))
        (rules nil))
    (dotimes (i max-repeats)
      (let* ((repeats (- max-repeats i))
             (pattern (apply 'concat (make-list repeats base-pattern)))
             (position (/ (float repeats) (float max-repeats)))
             (pattern-color
              (cond
               ((eq algo 'basic) (htc-generate-gradient-color color position))
               ((eq algo 'hsl) (htc-generate-hsl-gradient-color color position))
               ((eq algo 'complementary) (htc-generate-complementary-gradient color repeats max-repeats))
               (t (htc-generate-hsl-gradient-color color position)))))
        (push (cons pattern pattern-color) rules)))
    (nreverse rules)))

;;;; Predefined Color Palettes

(defun htc-generate-earth-tones-rules (base-pattern max-repeats)
  "Generate rules using a warm earth tones palette for a natural, organic look."
  (let ((palette '("#faf6f2" ;; Very light cream
                   "#f7ede2" ;; Light peach
                   "#f4e5d3" ;; Pale tan
                   "#f0dcc3" ;; Light sand
                   "#edd3b4" ;; Medium sand
                   "#e9cba5" ;; Pale gold
                   "#e6c296" ;; Light caramel
                   "#e2ba86")) ;; Medium caramel
        (rules nil))
    (dotimes (i max-repeats)
      (let* ((repeats (- max-repeats i))
             (pattern (apply 'concat (make-list repeats base-pattern)))
             (color-index (mod (- max-repeats repeats) (length palette)))
             (color (nth color-index palette)))
        (push (cons pattern color) rules)))
    (nreverse rules)))

;;;; High Contrast Accessibility Palette

(defun htc-generate-high-contrast-rules (base-pattern max-repeats)
  "Generate rules with high contrast colors suitable for accessibility needs."
  (let ((palette '("#ffffff" ;; White
                   "#f2f2f2" ;; Very light gray
                   "#e6e6e6" ;; Light gray
                   "#d9d9d9" ;; Medium light gray
                   "#cccccc" ;; Medium gray
                   "#bfbfbf" ;; Gray
                   "#b3b3b3" ;; Medium dark gray
                   "#a6a6a6")) ;; Dark gray
        (rules nil))
    (dotimes (i max-repeats)
      (let* ((repeats (- max-repeats i))
             (pattern (apply 'concat (make-list repeats base-pattern)))
             (color-index (mod (- max-repeats repeats) (length palette)))
             (color (nth color-index palette)))
        (push (cons pattern color) rules)))
    (nreverse rules)))

;;;; Purple to Pink Gradient Palette

(defun htc-generate-purple-pink-rules (base-pattern max-repeats)
  "Generate rules using a purple to pink gradient for a vibrant, creative look."
  (let ((palette '("#f9f0ff" ;; Very light lavender
                   "#f3e5fa" ;; Light lavender
                   "#eddaf5" ;; Pale lavender
                   "#e7cff0" ;; Soft lavender
                   "#e1c4eb" ;; Light purple
                   "#dbbae6" ;; Soft purple
                   "#d5afe1" ;; Medium purple
                   "#cfa4dc")) ;; Pale pink-purple
        (rules nil))
    (dotimes (i max-repeats)
      (let* ((repeats (- max-repeats i))
             (pattern (apply 'concat (make-list repeats base-pattern)))
             (color-index (mod (- max-repeats repeats) (length palette)))
             (color (nth color-index palette)))
        (push (cons pattern color) rules)))
    (nreverse rules)))

;;;; Monochromatic Gray Palette

(defun htc-generate-mono-gray-rules (base-pattern max-repeats)
  "Generate rules using a monochromatic grayscale palette for a professional, neutral look."
  (let ((palette '("#fafafa" ;; Nearly white
                   "#f5f5f5" ;; Very light gray
                   "#f0f0f0" ;; Light gray
                   "#ebebeb" ;; Pale gray
                   "#e6e6e6" ;; Light silver
                   "#e0e0e0" ;; Silver
                   "#dbdbdb" ;; Medium light gray
                   "#d6d6d6")) ;; Medium gray
        (rules nil))
    (dotimes (i max-repeats)
      (let* ((repeats (- max-repeats i))
             (pattern (apply 'concat (make-list repeats base-pattern)))
             (color-index (mod (- max-repeats repeats) (length palette)))
             (color (nth color-index palette)))
        (push (cons pattern color) rules)))
    (nreverse rules)))

;;;; Dark Mode Palette

(defun htc-generate-dark-mode-rules (base-pattern max-repeats)
  "Generate rules using a dark mode palette with lighter text on dark backgrounds."
  (let ((palette '("#2d3748" ;; Very dark blue-gray
                   "#323c4e" ;; Dark blue-gray
                   "#374155" ;; Medium dark blue-gray
                   "#3c465c" ;; Dark slate
                   "#414b62" ;; Medium slate
                   "#465069" ;; Slate
                   "#4b556f" ;; Medium light slate
                   "#505a76")) ;; Light slate
        (rules nil))
    (dotimes (i max-repeats)
      (let* ((repeats (- max-repeats i))
             (pattern (apply 'concat (make-list repeats base-pattern)))
             (color-index (mod (- max-repeats repeats) (length palette)))
             (color (nth color-index palette)))
        (push (cons pattern color) rules)))
    (nreverse rules)))

;;;; Autumn Colors Palette

(defun htc-generate-autumn-rules (base-pattern max-repeats)
  "Generate rules using autumn-inspired colors for a warm, seasonal look."
  (let ((palette '("#fff8f0" ;; Very light cream
                   "#fff1e0" ;; Pale orange
                   "#ffe9d1" ;; Light peach
                   "#ffe2c2" ;; Pale amber
                   "#ffdab3" ;; Light amber
                   "#ffd3a3" ;; Pale gold
                   "#ffcb94" ;; Light gold
                   "#ffc485")) ;; Medium gold
        (rules nil))
    (dotimes (i max-repeats)
      (let* ((repeats (- max-repeats i))
             (pattern (apply 'concat (make-list repeats base-pattern)))
             (color-index (mod (- max-repeats repeats) (length palette)))
             (color (nth color-index palette)))
        (push (cons pattern color) rules)))
    (nreverse rules)))

(defun htc-generate-modern-ui-rules (base-pattern max-repeats)
  "Generate rules using a modern UI color palette with subtle blues and grays."
  (let ((palette '("#f7f9fc" ;; Very light blue-gray
                   "#edf2f7" ;; Light blue-gray
                   "#e2e8f0" ;; Pale blue-gray
                   "#cbd5e0" ;; Light steel blue
                   "#a0aec0" ;; Steel blue
                   "#718096" ;; Medium steel blue
                   "#4a5568" ;; Dark steel blue
                   "#2d3748")) ;; Very dark blue-gray
        (rules nil))
    (dotimes (i max-repeats)
      (let* ((repeats (- max-repeats i))
             (pattern (apply 'concat (make-list repeats base-pattern)))
             (color-index (mod (- max-repeats repeats) (length palette)))
             (color (nth color-index palette)))
        (push (cons pattern color) rules)))
    (nreverse rules)))

(defun htc-generate-mono-blue-rules (base-pattern max-repeats)
  "Generate rules using a monochromatic blue palette for a professional look."
  (let ((palette '("#f0f5fa" ;; Lightest blue
                   "#e1eefa" ;; Very light blue
                   "#d2e7fa" ;; Light blue
                   "#c2e0fa" ;; Pale blue
                   "#b3d9fa" ;; Sky blue
                   "#a3d2fa" ;; Medium light blue
                   "#94cbfa" ;; Blue
                   "#84c4fa")) ;; Medium blue
        (rules nil))
    (dotimes (i max-repeats)
      (let* ((repeats (- max-repeats i))
             (pattern (apply 'concat (make-list repeats base-pattern)))
             (color-index (mod (- max-repeats repeats) (length palette)))
             (color (nth color-index palette)))
        (push (cons pattern color) rules)))
    (nreverse rules)))

(defun htc-generate-green-blue-rules (base-pattern max-repeats)
  "Generate rules using a green to blue gradient for a calming effect."
  (let ((palette '("#e6f7ee" ;; Very light green
                   "#d7f2e4" ;; Light green
                   "#c8edda" ;; Pale green
                   "#b9e8d3" ;; Mint
                   "#aae3d9" ;; Pale teal
                   "#9bdee0" ;; Light cyan
                   "#8cd9e6" ;; Pale azure
                   "#7dd4ec")) ;; Light blue
        (rules nil))
    (dotimes (i max-repeats)
      (let* ((repeats (- max-repeats i))
             (pattern (apply 'concat (make-list repeats base-pattern)))
             (color-index (mod (- max-repeats repeats) (length palette)))
             (color (nth color-index palette)))
        (push (cons pattern color) rules)))
    (nreverse rules)))

(defun htc-generate-solarized-rules (base-pattern max-repeats)
  "Generate rules using a Solarized-inspired color palette that's easy on the eyes."
  (let ((palette '("#fdf6e3" ;; Solarized light base
                   "#eee8d5" ;; Solarized light secondary
                   "#e0dbc8" ;; Muted beige
                   "#d3ccba" ;; Light tan
                   "#c6bdad" ;; Pale brown
                   "#b8ae9f" ;; Medium tan
                   "#aba092" ;; Medium brown
                   "#9d9184")) ;; Dark tan
        (rules nil))
    (dotimes (i max-repeats)
      (let* ((repeats (- max-repeats i))
             (pattern (apply 'concat (make-list repeats base-pattern)))
             (color-index (mod (- max-repeats repeats) (length palette)))
             (color (nth color-index palette)))
        (push (cons pattern color) rules)))
    (nreverse rules)))

;;;; HTML Processing Functions

(defun htc-apply-row-style (row-start row-attributes color)
  "Apply a background COLOR to the row starting at ROW-START with ROW-ATTRIBUTES."
  (goto-char row-start)
  (kill-line)
  (insert (format "<tr%s style=\"background: %s\">\n" row-attributes color)))

(defun htc-highlight-row-by-rules (row-start row-end row-attributes element)
  "Highlight a row based on ELEMENT ('th or 'td) keyword rules within ROW-START to ROW-END.
Uses the rules defined in `htc-highlight-rules'. Exits on the first matching rule."
  (let ((rules (cdr (assoc element htc-highlight-rules))))
    (catch 'found-match
      (dolist (rule rules)
        (let ((keyword (car rule))
              (color (cdr rule)))
          (when (save-excursion
                  (and (re-search-forward (format "<%s.*>%s.*</%s>" element keyword element) row-end t)
                       (goto-char row-start)))
            (htc-apply-row-style row-start row-attributes color)
            (throw 'found-match t)))))))

;;;; Interactive Functions

(defun htc-colorize-html-buffer ()
  "Process the current buffer, assumed to be HTML, adding background styles to 
table rows containing keywords in <td> or <th> elements as defined in `htc-highlight-rules'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<table.*>" nil t)
      (let ((table-start (point))
            (table-end (save-excursion
                         (when (re-search-forward "</table>" nil t)
                           (point)))))
        (when table-end
          (save-restriction
            (narrow-to-region table-start table-end)
            (goto-char (point-min))
            (while (re-search-forward "<tr\\(.*\\)>" nil t)
              (let ((row-start (match-beginning 0))
                    (row-attributes (match-string 1))
                    (row-end (save-excursion (search-forward "</tr>"))))
                (htc-highlight-row-by-rules row-start row-end row-attributes 'th)
                (htc-highlight-row-by-rules row-start row-end row-attributes 'td)))))))))

(defun htc-colorize-html-file (file)
  "Colorize HTML tables in FILE according to the current highlighting rules."
  (interactive "fHTML file to colorize: ")
  (with-temp-buffer
    (insert-file-contents file)
    (htc-colorize-html-buffer)
    (write-region (point-min) (point-max) file)))

(defun htc-colorize-current-html-export ()
  "Find the HTML file exported from the current Org file and colorize its tables."
  (interactive)
  (when (and (buffer-file-name) (string-match "\\.org$" (buffer-file-name)))
    (let* ((org-file (buffer-file-name))
           (html-file (concat (file-name-sans-extension org-file) ".html")))
      (when (file-exists-p html-file)
        (htc-colorize-html-file html-file)
        (message "Tables in %s have been colorized" html-file)))))

;;;; Setup Functions
(defun htc-setup-pastel-theme ()
  "Set up pastel highlighting rules."
  (interactive)
  (setq htc-highlight-rules
        `((th . (("Requirement" . "#ebbdbd"))) ;; Soft blue for headers
          (td . (,@(htc-generate-pastel-rules "- " 8)
                 ("TODO" . "#ffd1d1")
                 ("DOING" . "#d1ffd3")
                 ("DONE" . "#d1d1ff"))))))

(defun htc-setup-emacs-default-theme ()
  "Set up Emacs default theme highlighting rules."
  (interactive)
  (setq htc-highlight-rules
        `((th . (("Requirement" . "#3465a4"))) ;; Classic Emacs blue for headers
          (td . (,@(htc-generate-emacs-default-rules "- " 8)
                 ("TODO" . "#ffb6b0")
                 ("DOING" . "#b0ffb6")
                 ("DONE" . "#b0b0ff"))))))

(defun htc-setup-emacs-adwaita-theme ()
  "Set up Emacs Adwaita theme highlighting rules."
  (interactive)
  (setq htc-highlight-rules
        `((th . (("Requirement" . "#4a90d9"))) ;; Adwaita blue for headers
          (td . (,@(htc-generate-emacs-adwaita-rules "- " 8)
                 ("TODO" . "#ffb6b0")
                 ("DOING" . "#b0ffb6")
                 ("DONE" . "#b0b0ff"))))))

(defun htc-setup-emacs-tango-theme ()
  "Set up Emacs Tango theme highlighting rules."
  (interactive)
  (setq htc-highlight-rules
        `((th . (("Requirement" . "#729fcf"))) ;; Tango blue for headers
          (td . (,@(htc-generate-emacs-tango-rules "- " 8)
                 ("TODO" . "#ef2929")
                 ("DOING" . "#8ae234")
                 ("DONE" . "#729fcf"))))))

(defun htc-setup-emacs-leuven-theme ()
  "Set up Emacs Leuven theme highlighting rules."
  (interactive)
  (setq htc-highlight-rules
        `((th . (("Requirement" . "#5180b3"))) ;; Leuven blue for headers
          (td . (,@(htc-generate-emacs-leuven-rules "- " 8)
                 ("TODO" . "#ff8f88")
                 ("DOING" . "#95ff8f")
                 ("DONE" . "#8f88ff"))))))

(defun htc-setup-earth-tones-theme ()
  "Set up earth tones highlighting rules."
  (interactive)
  (setq htc-highlight-rules
        `((th . (("Requirement" . "#b86125"))) ;; Warm brown for headers
          (td . (,@(htc-generate-earth-tones-rules "- " 8)
                 ("TODO" . "#ffb6b0")
                 ("DOING" . "#b0ffb6")
                 ("DONE" . "#b0b0ff"))))))

(defun htc-setup-high-contrast-theme ()
  "Set up high contrast highlighting rules for accessibility."
  (interactive)
  (setq htc-highlight-rules
        `((th . (("Requirement" . "#000000"))) ;; Black for headers on white
          (td . (,@(htc-generate-high-contrast-rules "- " 8)
                 ("TODO" . "#ff8a80")
                 ("DOING" . "#80ff8a")
                 ("DONE" . "#8a80ff"))))))

(defun htc-setup-purple-pink-theme ()
  "Set up purple-pink gradient highlighting rules."
  (interactive)
  (setq htc-highlight-rules
        `((th . (("Requirement" . "#9c27b0"))) ;; Purple for headers
          (td . (,@(htc-generate-purple-pink-rules "- " 8)
                 ("TODO" . "#ffb6b0")
                 ("DOING" . "#b0ffb6")
                 ("DONE" . "#b0b0ff"))))))

(defun htc-setup-mono-gray-theme ()
  "Set up monochromatic gray highlighting rules."
  (interactive)
  (setq htc-highlight-rules
        `((th . (("Requirement" . "#616161"))) ;; Dark gray for headers
          (td . (,@(htc-generate-mono-gray-rules "- " 8)
                 ("TODO" . "#ffb6b0")
                 ("DOING" . "#b0ffb6")
                 ("DONE" . "#b0b0ff"))))))

(defun htc-setup-dark-mode-theme ()
  "Set up dark mode highlighting rules."
  (interactive)
  (setq htc-highlight-rules
        `((th . (("Requirement" . "#90caf9"))) ;; Light blue for headers
          (td . (,@(htc-generate-dark-mode-rules "- " 8)
                 ("TODO" . "#b71c1c")
                 ("DOING" . "#1cb71c")
                 ("DONE" . "#1c1cb7"))))))

(defun htc-setup-autumn-theme ()
  "Set up autumn colors highlighting rules."
  (interactive)
  (setq htc-highlight-rules
        `((th . (("Requirement" . "#e65100"))) ;; Dark orange for headers
          (td . (,@(htc-generate-autumn-rules "- " 8)
                 ("TODO" . "#ffb6b0")
                 ("DOING" . "#b0ffb6")
                 ("DONE" . "#b0b0ff"))))))

(defun htc-setup-solarized-theme ()
  "Set up Solarized-inspired highlighting rules."
  (interactive)
  (setq htc-highlight-rules
        `((th . (("Requirement" . "#268bd2"))) ;; Solarized blue for headers
          (td . (,@(htc-generate-solarized-rules "- " 8)
                 ("TODO" . "#ffb6b0")
                 ("DOING" . "#b0ffb6")
                 ("DONE" . "#b0b0ff"))))))

(defun htc-setup-modern-theme ()
  "Set up modern UI highlighting rules."
  (interactive)
  (setq htc-highlight-rules
        `((th . (("Requirement" . "#4a5568"))) ;; Dark blue-gray for headers
          (td . (,@(htc-generate-modern-ui-rules "- " 8)
                 ("TODO" . "#ffe9e7")
                 ("DOING" . "#e7ffe9")
                 ("DONE" . "#eeecff"))))))

;;;; Advanced Custom Theme Creator

(defun htc-create-custom-theme (name base-color &optional secondary-colors)
  "Create a custom theme named NAME with BASE-COLOR as primary color.
Optionally specify SECONDARY-COLORS for TODO, DOING, DONE states."
  (interactive "sTheme name: \nsBase color (hex): ")
  (let* ((todo-color (or (nth 0 secondary-colors) "#ffb6b0"))
         (doing-color (or (nth 1 secondary-colors) "#b0ffb6"))
         (done-color (or (nth 2 secondary-colors) "#b0b0ff"))
         (func-name (intern (format "htc-setup-%s-theme" (downcase name))))
         (func-doc (format "Set up %s highlighting rules." name)))
    ;; Create the setup function dynamically
    (eval `(defun ,func-name ()
             ,func-doc
             (interactive)
             (setq htc-highlight-rules
                   '((th . (("Requirement" . ,base-color)))
                     (td . (,@(htc-generate-hsl-gradient-color ,base-color 8)
                            ("TODO" . ,todo-color)
                            ("DOING" . ,doing-color)
                            ("DONE" . ,done-color)))))))
    (message "Created custom theme '%s'. Use it with M-x %s" 
             name (symbol-name func-name))))

;;;; Theme Preview and Selection Interface

(defun htc-preview-theme (theme-setup-func)
  "Preview a theme by applying it temporarily to a sample HTML table."
  (interactive
   (list (intern (completing-read "Select theme to preview: "
                                  '("htc-setup-solarized-theme"
                                    "htc-setup-modern-theme"
                                    "htc-setup-earth-tones-theme"
                                    "htc-setup-high-contrast-theme"
                                    "htc-setup-purple-pink-theme"
                                    "htc-setup-mono-gray-theme"
                                    "htc-setup-dark-mode-theme"
                                    "htc-setup-autumn-theme")))))
  (let ((original-rules htc-highlight-rules)
        (preview-buffer (get-buffer-create "*HTML Table Theme Preview*")))
    (funcall theme-setup-func)
    (with-current-buffer preview-buffer
      (erase-buffer)
      (insert "<html><body>
<table border=\"1\">
<tr><th>Requirement</th><th>Status</th><th>Notes</th></tr>
<tr><td>Feature A</td><td>TODO</td><td>Needs implementation</td></tr>
<tr><td>Feature B</td><td>DOING</td><td>In progress</td></tr>
<tr><td>Feature C</td><td>DONE</td><td>Completed last week</td></tr>
<tr><td>- Item 1</td><td>Notes</td><td>Single level</td></tr>
<tr><td>- - Item 2</td><td>Notes</td><td>Two levels</td></tr>
<tr><td>- - - Item 3</td><td>Notes</td><td>Three levels</td></tr>
<tr><td>- - - - Item 4</td><td>Notes</td><td>Four levels</td></tr>
<tr><td>- - - - - Item 5</td><td>Notes</td><td>Five levels</td></tr>
</table>
</body></html>")
      (htc-colorize-html-buffer)
      (html-mode)
      (switch-to-buffer preview-buffer))
    (message "Previewing theme. Press q to close preview and restore original theme.")
    (let ((restore-theme (lambda ()
                           (interactive)
                           (setq htc-highlight-rules original-rules)
                           (kill-buffer preview-buffer)
                           (remove-hook 'kill-buffer-hook restore-theme t))))
      (with-current-buffer preview-buffer
        (local-set-key (kbd "q") restore-theme)
        (add-hook 'kill-buffer-hook restore-theme nil t)))))

;;;; Mode Definition

(defvar htc-theme-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'htc-select-theme)
    (define-key map (kbd "p") 'htc-select-and-preview-theme)
    (define-key map (kbd "r") 'htc-apply-random-theme)
    (define-key map (kbd "l") 'htc-list-available-themes)
    (define-key map (kbd "c") 'htc-create-custom-theme)
    map)
  "Keymap for HTML Table Colorizer theme commands.")

(defvar htc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t c") 'htc-colorize-html-buffer)
    (define-key map (kbd "C-c C-t f") 'htc-colorize-html-file)
    (define-key map (kbd "C-c C-t o") 'htc-colorize-current-html-export)
    (define-key map (kbd "C-c C-t t") (cons "Theme Menu" htc-theme-map))
    map)
  "Keymap for HTML Table Colorizer commands.")

(easy-menu-define htc-mode-menu htc-mode-map
  "Menu for HTML Table Colorizer mode."
  '("HTC"
    ["Colorize Buffer" htc-colorize-html-buffer t]
    ["Colorize File" htc-colorize-html-file t]
    ["Colorize Org Export" htc-colorize-current-html-export t]
    "---"
    ("Themes"
     ["Select Theme" htc-select-theme t]
     ["Preview Theme" htc-select-and-preview-theme t]
     ["Random Theme" htc-apply-random-theme t]
     ["List All Themes" htc-list-available-themes t]
     ["Create Custom Theme" htc-create-custom-theme t])))

(define-minor-mode html-table-colorizer-mode
  "Minor mode for colorizing HTML tables based on content."
  :lighter " HTC"
  :keymap htc-mode-map
  :group 'html-table-colorizer
  (if html-table-colorizer-mode
      (message "HTML Table Colorizer mode enabled")
    (message "HTML Table Colorizer mode disabled")))

;;;; Org Export Hook

(defun htc-enable-org-html-export-hook ()
  "Add hook to automatically colorize tables after org-html export."
  (interactive)
  (add-hook 'org-export-html-final-hook 'htc-colorize-current-html-export))

(defun htc-disable-org-html-export-hook ()
  "Remove hook to automatically colorize tables after org-html export."
  (interactive)
  (remove-hook 'org-export-html-final-hook 'htc-colorize-current-html-export))

(provide 'html-table-colorizer)

;;; html-table-colorizer.el ends here
