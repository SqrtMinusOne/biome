;;; biome-grid.el --- Display results of open meteo queries -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Korytov Pavel

;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Homepage: https://github.com/SqrtMinusOne/biome

;; This file is NOT part of GNU Emacs.

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

;;; Commentary:

;; Display results of open meteo queries with `tabulated-list'.

;;; Code:
(require 'cl-lib)
(require 'iso8601)
(require 'ct)
(require 'seq)
(require 'tabulated-list)
(require 'transient)
(require 'biome-query)

(defcustom biome-grid-display-units t
  "Display units in the grid."
  :type 'boolean
  :group 'biome)

(defcustom biome-grid-highlight-current t
  "Highlight current hour or day.

This requires time formatted as iso8601."
  :type 'boolean
  :group 'boolean)

(defface biome-grid-highlight-current-face
  '((t :inherit highlight))
  "Highlight current hour or day."
  :group 'biome)

(defun biome-grid--format-units (format)
  "Add derived units to FORMAT.

FORMAT is an expression as defined by `biome-grid-format'."
  (nreverse
   (cl-reduce (lambda (acc def)
                (push def acc)
                (let ((unit (nth 0 def))
                      (format-def (nth 2 def)))
                  (when (and unit (eq (car-safe format-def) 'gradient))
                    (pcase unit
                      ("°C" (push `("°F" ,(nth 1 def)
                                    (gradient
                                     . ,(mapcar (lambda (c)
                                                  (cons (+ (* 1.8 (car c)) 32) (cdr c)))
                                                (cdr format-def))))
                                  acc))
                      ("m/s"
                       (push `("km/h" ,(nth 1 def)
                               (gradient
                                . ,(mapcar (lambda (c)
                                             (cons (* 3.6 (car c)) (cdr c)))
                                           (cdr format-def))))
                             acc)
                       (push `("mph" ,(nth 1 def)
                               (gradient
                                . ,(mapcar (lambda (c)
                                             (cons (* 2.237 (car c)) (cdr c)))
                                           (cdr format-def))))
                             acc)
                       (push `("knots" ,(nth 1 def)
                               (gradient
                                . ,(mapcar (lambda (c)
                                             (cons (* 1.944 (car c)) (cdr c)))
                                           (cdr format-def))))
                             acc))
                      ("mm" (push `("inch" ,(nth 1 def)
                                    (gradient
                                     . ,(mapcar (lambda (c)
                                                  (cons (/ (car c) 25.4) (cdr c)))
                                                (cdr format-def))))
                                  acc))
                      ("cm" (push `("inch" ,(nth 1 def)
                                    (gradient
                                     . ,(mapcar (lambda (c)
                                                  (cons (/ (car c) 2.54) (cdr c)))
                                                (cdr format-def))))
                                  acc)))))
                acc)
              format :initial-value nil)))

(defcustom biome-grid-format
  (biome-grid--format-units
   '(("°C" nil (gradient
                (-40 . "#ae64a0")
                (-30 . "#9488d2")
                (-15 . "#90cfd2")
                (-5 . "#66adbb")
                (5 . "#508c40")
                (15 . "#aba10e")
                (25 . "#f39506")
                (35 . "#bf4112")
                (40 . "#8a2b0a")))
     ("hPa" nil (gradient
                 (980 . "#006794")
                 (1005 . "#469998")
                 (1015 . "#b1b09d")
                 (1025 . "#a36f40")
                 (1030 . "#a0522c")))
     ("mm" nil (gradient
                (0 . "#3b7ba0")
                (1.5 . "#3a98a2")
                (5 . "#35a17f")
                (8.5 . "#47a43e")
                (15 . "#a0a23b")
                (25 . "#a53856")
                (30 . "#a43a9a")))
     ("cm" nil (gradient
                (0 . "#3b7ba0")
                (2 . "#3a98a2")
                (5 . "#35a17f")
                (10 . "#47a43e")
                (25 . "#a0a23b")
                (50 . "#a53856")
                (100 . "#a43a9a")))
     ("%%" "humidity" (gradient
                       (0 . "#ae6e38")
                       (40 . "#ae8a38")
                       (60 . "#35a17f")
                       (85 . "#388fae")
                       (100 . "#384774")))
     ("%%" "probability" (gradient
                          (0 . "#ae6e38")
                          (25 . "#ae8a38")
                          (50 . "#35a17f")
                          (75 . "#388fae")
                          (100 . "#384774")))
     ("%%" "cloud_cover" (gradient
                         (0 . "#7f7655")
                         (50 . "#8d9291")
                         (100 . "#d2d2cb")))
     ("m/s" nil (gradient
                 (0 . "#606fb5")
                 (4 . "#4a9296")
                 (8 . "#4ca34d")
                 (12 . "#a08740")
                 (18 . "#8d405c")
                 (40 . "#0b1c8f")))
     ("W/m²" nil (gradient
                  (0 . "#6e6e6e")
                  (200 . "#ce5152")
                  (400 . "#e5875d")
                  (600 . "#f0a56c")
                  (800 . "#fed895")
                  (1000 . "#fff6b5")))
     ("J/kg" nil (gradient
                  (250 . "#6b6d6f")
                  (500 . "#24608b")
                  (1000 . "#45a921")
                  (1500 . "##979e33")
                  (2000 . "#9d773e")
                  (2500 . "#a03735")
                  (3500 . "#a63964")
                  (5000 . "#984497")))
     ("MJ/m²" nil (gradient
                   (0 . "#6e6e6e")
                   (7 . "#ce5152")
                   (13 . "#e5875d")
                   (18 . "#f0a56c")
                   (25 . "#fed895")
                   (35 . "#fff6b5")))
     (nil "uv_index" (gradient
                      (0 . "#6b6d6f")
                      (1 . "#24608b")
                      (2 . "#45a921")
                      (4 . "#979e33")
                      (6 . "#9d773e")
                      (8 . "#a03735")
                      (10 . "#a63964")
                      (11 . "#984497")))
     ("m" "wave_height" (gradient
                         (0 . "#329eba")
                         (0.75 . "#3080a4")
                         (1.25 . "#3465a5")
                         (1.75 . "#3852a8")
                         (3 . "#763343")
                         (6 . "#b94957")
                         (9 . "#c0a29d")))
     ("m" "height" (gradient
                    (0 . "#bdc6c4")
                    (500 . "#a3c4bf")
                    (1250 . "#448478")
                    (2000 . "#265252")
                    (3000 . "#214048")
                    (9000 . "#1d2642")))
     ("m" "visibility" (gradient
                        (0 . "#fc6870")
                        (1500 . "#a35898")
                        (3000 . "#9b5861")
                        (6000 . "#58609e")
                        (10000 . "#527f84")
                        (16000 . "#49b04c")
                        (24140.0 . "#5b9e55")))
     ("m" "depth" (gradient
                   (0 . "#465397")
                   (0.1 . "#3e85a5")
                   (0.2 . "#599b49")
                   (0.5 . "#a0a33e")
                   (1 . "#ab7c3e")
                   (3 . "#ae4252")
                   (5 . "#b13c74")))
     ("m³/m³" nil (gradient
                   (0 . "#ae6e38")
                   (0.1 . "#ae8a38")
                   (0.15 . "#35a17f")
                   (0.25 . "#388fae")
                   (0.3 . "#384774")
                   (0.4 . "#984497") ))
     ("kPa" nil (gradient
                 (0 . "#3e4bf8")
                 (0.4 . "#58ba64")
                 (1.0 . "#b7ba58")
                 (1.4 . "#ba7158")))
     ("EAQI" nil (gradient
                  (0 . "#465397")
                  (10 . "#3e85a5")
                  (20 . "#599b49")
                  (40 . "#a0a33e")
                  (60 . "#ab7c3e")
                  (80 . "#ae4252")
                  (100 . "#b13c74")))
     ("USAQI" nil (gradient
                   (0 . "#465397")
                   (50 . "#3e85a5")
                   (100 . "#599b49")
                   (150 . "#a0a33e")
                   (200 . "#ab7c3e")
                   (300 . "#ae4252")
                   (500 . "#b13c74")))
     ("μg/m³" "pm2" (gradient
                     (0 . "#465397")
                     (12 . "#599b49")
                     (20 . "#a0a33e")
                     (55 . "#ab7c3e")
                     (150 . "#ae4252")
                     (250 . "#b13c74")))
     ("μg/m³" "pm10" (gradient
                      (0 . "#465397")
                      (55 . "#599b49")
                      (100 . "#a0a33e")
                      (200 . "#ab7c3e")
                      (255 . "#ae4252")
                      (355 . "#b13c74")))
     ("μg/m³" "carbon_monoxide" (gradient
                                 (0 . "#7c7c7c")
                                 (55 . "#74746a")
                                 (100 . "#969153")
                                 (250 . "#5d4c28")
                                 (500 . "#2f1e1e")
                                 (1000 . "#841b1b")))
     ("μg/m³" "nitrogen_dioxide" (gradient
                                  (0 . "#465397")
                                  (2 . "#599b49")
                                  (5 . "#a0a33e")
                                  (25 . "#ab7c3e")
                                  (50 . "#ae4252")
                                  (100 . "#b13c74")))
     ("μg/m³" "sulphur_dioxide" (gradient
                                 (0 . "#465397")
                                 (50 . "#599b49")
                                 (150 . "#a0a33e")
                                 (300 . "#ab7c3e")
                                 (350 . "#ae4252")
                                 (500 . "#b13c74")))
     ("μg/m³" "ozone" (gradient
                       (0 . "#465397")
                       (50 . "#599b49")
                       (75 . "#a0a33e")
                       (120 . "#ab7c3e")
                       (140 . "#ae4252")
                       (240 . "#b13c74")))
     (nil "is_day" is-day)
     ("wmo code" nil wmo-code)
     ("iso8601" nil date)
     ("°" nil direction)))
  "Format units in the grid.

This is a list with the following keys:
- unit;
- parameter (regexp);
- format defintion.

The format definition is one of the following:
- (gradient (value . color) ...);
- `is-day';
- `wmo-code';
- `direction'."
  :group 'biome
  :type '(repeat (list
                  (choice :tag "Unit" (const :tag "No unit" nil) string)
                  (choice :tag "Parameter (regexp)" (const :tag "No parameter" nil) string)
                  (choice
                   (cons (const gradient)
                         (alist :key-type (number :tag "Value")
                                :value-type (string :tag "HEX color")))
                   (const :tag "Is day" is-day)
                   (const :tag "WMO code" wmo-code)
                   (const :tag "Date" date)
                   (const :tag "Direction" direction)))))

(defcustom biome-grid-wmo-codes
  '((0 "☀️" "Clear sky")
    (1 "🌤️" "Mainly clear")
    (2 "🌥️" "Partly cloudy")
    (3 "☁️" "Overcast")
    (45 "🌫️" "Fog")
    (48 "🌫️" "Depositing rime fog")
    (51 "💧" "Drizzle (light)")
    (53 "💧" "Drizzle (moderate)")
    (55 "💧" "Drizzle (dense)")
    (56 "🧊" "Freezing drizzle (light)")
    (57 "🧊" "Freezing drizzle (moderate)")
    (61 "🌧️" "Rain (light)")
    (63 "🌧️" "Rain (moderate)")
    (65 "🌧️" "Rain (heavy)")
    (66 "🧊" "Freezing rain (light)")
    (67 "🧊" "Freezing rain (heavy)")
    (71 "🌨️" "Snow (light)")
    (73 "🌨️" "Snow (moderate)")
    (75 "🌨️" "Snow (heavy)")
    (77 "🌨️" "Snow grains")
    (80 "🌧️" "Rain showers (light)")
    (81 "🌧️" "Rain showers (moderate)")
    (82 "🌧️" "Rain showers (violent)")
    (85 "🌧️" "Show showers (light)")
    (86 "🌧️" "Show showers (heavy)")
    (95 "⛈️" "Thunderstorm")
    (96 "⛈️" "Thunderstorm with hail (light)")
    (99 "⛈️" "Thunderstorm with hail (heavy)"))
  "Descriptions for WMO weather codes.

The defaults values are taken from open-meteo docs."
  :group 'biome
  :type '(alist :key-type number :value-type (list string string)))

(defcustom biome-grid-directions
  '((0 "↑ N")
    (22.5 "↑ NNE")
    (45 "↗ NE")
    (67.5 "↗ ENE")
    (90 "→ E")
    (112.5 "→ ESE")
    (135 "↘ SE")
    (157.5 "↘ SSE")
    (180 "↓ S")
    (202.5 "↓ SSW")
    (225 "↙ SW")
    (247.5 "↙ WSW")
    (270 "← W")
    (292.5 "← WNW")
    (315 "↖ NW")
    (337.5 "↖ NNW")
    (360 "↑ N"))
  "Descriptions for directions."
  :group 'biome
  :type '(repeat (list (number :tag "Value (0-360)")
                       (string :tag "Description"))))

(defcustom biome-grid-is-day-format '("🌙 night" "☀️ day")
  "Format for is-day values."
  :group 'biome
  :type '(list (string :tag "Night") (string :tag "Day")))

(defcustom biome-grid-wmo-show-emoji t
  "Show emoji for WMO weather codes."
  :type 'boolean
  :group 'biome)

(defcustom biome-grid-date-format "%Y-%m-%d"
  "Format for date strings.

See `format-time-string' for possible format values."
  :type 'string
  :group 'biome)

(defcustom biome-grid-datetime-format "%Y-%m-%d %H:%M"
  "Format for datetime strings.

See `format-time-string' for possible format values."
  :type 'string
  :group 'biome)

(defvar-local biome-grid--columns-display nil
  "Which columns to display in the grid.

This is set by the first run of `biome-grid--set-list'.")

(defvar-local biome-grid--current-query nil
  "Current query for the grid.")

(defvar-local biome-grid--current-results nil
  "Current results for the grid.")

(defun biome-grid--blend-colors (c1 c2 val)
  "Blend colors C1 and C2 by VAL.

C1 and C2 are hex RGS strings, VAL is a number between 0 and 1."
  (let ((color1 (ct-get-rgb c1))
        (color2 (ct-get-rgb c2)))
    (apply #'ct-make-rgb
           (cl-loop for v1 in color1
                    for v2 in color2
                    collect (+ (* (- 1 val) v1) (* val v2))))))

(defun biome-grid--format-gradient (value def col-width)
  "Format VALUE with gradient DEF.

DEF is a list of (value . color) pairs.  COL-WIDTH is the width of the
column."
  (setq value (float value))
  (let* ((background-color
          (cl-loop for (border1 . color1) in def
                   for (border2 . color2) in (cdr def)
                   if (< value border1) return color1
                   if (and (>= value border1) (<= value border2))
                   return (biome-grid--blend-colors
                           color1 color2 (/ (- value border1) (- border2 border1)))
                   finally return color2))
         (foreground-color (if (ct-light-p background-color 65) "#000000" "#ffffff")))
    (propertize
     (format (format "%%%ds" col-width) value)
     'face `(:background ,background-color :foreground ,foreground-color))))

(defun biome-grid--format-wmo-code (value)
  "Format WMO code.

VALUE is a WMO number as defined in `biome-grid-wmo-codes'."
  (let ((format (alist-get value biome-grid-wmo-codes)))
    (if format
        (if biome-grid-wmo-show-emoji
            (format "%s %s" (nth 0 format) (nth 1 format))
          (format "%s" (nth 1 format)))
      (format "%s" value))))

(defun biome-grid--format-direction (value)
  "Format direction.

VALUE is a number from 0 to 360."
  (let ((desc (cl-loop for (border1 desc1) in biome-grid-directions
                       for (border2 desc2) in (cdr biome-grid-directions)
                       if (and (>= value border1) (<= value border2))
                       return (if (< (- value border1) (- border2 value))
                                  desc1 desc2)
                       finally return desc2)))
    (format "%-5s %d" desc value)))

(defun biome-grid--format-is-day (value)
  "Format is-day.

VALUE is 0 or 1."
  (nth value biome-grid-is-day-format))

(defun biome-grid--get-format-def (col-key unit)
  "Get format definition for COL-KEY and UNIT."
  (cl-loop for (unit-def col-key-def format-def) in biome-grid-format
           if (and (or (null unit-def) (equal unit unit-def))
                   (or (null col-key-def) (string-match-p col-key-def
                                                          (symbol-name col-key))))
           return format-def))

(defun biome-grid--format-date (value)
  "Format VALUE as iso8601 date or datetime."
  (let* ((decoded-time (iso8601-parse value))
         (has-time (fixnump (car decoded-time))))
    (if has-time
        (format-time-string biome-grid-datetime-format (encode-time decoded-time))
      (setf (nth 0 decoded-time) 0
            (nth 1 decoded-time) 0
            (nth 2 decoded-time) 0)
      (format-time-string biome-grid-date-format (encode-time decoded-time)))))

(defun biome-grid--prepare-entries (entries col-key unit)
  "Prepare entries for display.

This function performs all changes that may affect the width of the
column.

ENTRIES is a list of values.  COL-KEY is the column key as given by
the API.  UNIT is the unit of the column."
  ;; Current weather is not a sequence.
  (unless (sequencep entries)
    (setq entries (list entries)))
  (let ((format-def (biome-grid--get-format-def col-key unit)))
    (mapcar
     (lambda (entry)
       (cond
        ((or (null entry) (equal entry "")) "")
        ((eq format-def 'wmo-code)
         (biome-grid--format-wmo-code entry))
        ((eq format-def 'direction)
         (biome-grid--format-direction entry))
        ((eq format-def 'is-day)
         (biome-grid--format-is-day entry))
        ((eq format-def 'date)
         (biome-grid--format-date entry))
        (t entry)))
     entries)))

(defun biome-grid--format-entries (entries col-key unit col-width)
  "Format entries for display.

This uses the width COL-WIDTH for each column, therefore it must not change
that width.

ENTRIES is a list of values.  COL-KEY is the column key as given by
the API.  UNIT is the unit of the column."
  (let ((format-def (biome-grid--get-format-def col-key unit)))
    (mapcar
     (lambda (entry)
       (cond
        ((or (equal entry "")) entry)
        ((eq (car-safe format-def) 'gradient)
         (biome-grid--format-gradient entry (cdr format-def) col-width))
        ((numberp entry) (format (format "%%%ds" col-width) entry))
        ((stringp entry) entry)
        (t (prin1-to-string entry))))
     entries)))

(defun biome-grid--get-col-witdh (col-name entries unit)
  "Get the width of the column COL-NAME.

ENTRIES is a list of values.  UNIT is the unit of the column."
  (let ((col-name-width (length col-name))
        (entry-width
         (or (cl-loop for entry in entries
                      maximize (length (if (numberp entry)
                                           (number-to-string entry) entry)))
             0)))
    ;; XXX this is necessary to compensate for emojis of different
    ;; actual width.  Forunately this doesn't break the formmating of
    ;; the grid (hail `tabulated-list', what a pleasant surprise)
    (cond ((and (equal unit "wmo code") biome-grid-wmo-show-emoji)
           (max col-name-width (+ 1 entry-width)))
          (t (max col-name-width (+ 1 entry-width))))))

(defun biome-grid--set-list (query results)
  "Set the list of the grid for the `biome-grid-mode' buffer.

QUERY is a form as defined by `biome-query-current'.  RESULTS is a
response of the API."
  (let* ((group (intern (alist-get :group query)))
         (group-units (intern (format "%s_units" (alist-get :group query))))
         (var-names (biome-query--get-var-names-cache))
         (data (seq-filter
                (lambda (group)
                  (or (null biome-grid--columns-display)
                      (nth 1 (alist-get (car group) biome-grid--columns-display))))
                (alist-get group results)))
         all-entries columns columns-display)
    (cl-loop for (key . values) in data
             for unit = (replace-regexp-in-string
                         (regexp-quote "%") "%%"
                         (alist-get key (alist-get group-units results)))
             for var-name = (biome-query--get-header (symbol-name key) var-names)
             for col-name = (if (and biome-grid-display-units
                                     (not (string-empty-p unit)))
                                (format "%s (%s)" var-name unit) var-name)
             for prepared-values = (biome-grid--prepare-entries values key unit)
             for col-width = (biome-grid--get-col-witdh col-name prepared-values unit)
             for entries = (biome-grid--format-entries prepared-values key unit col-width)
             do (push (list col-name col-width nil) columns)
             do (push entries all-entries)
             do (push (list key col-name t) columns-display))
    (setq-local
     biome-grid--current-query (copy-tree query)
     biome-grid--current-results results
     tabulated-list-format (vconcat (nreverse columns))
     tabulated-list-entries
     (seq-map-indexed
      (lambda (entries i)
        (list i (vconcat entries)))
      (cl-reduce (lambda (acc entries)
                   (cl-loop for entry in entries
                            for i from 0
                            do (setf (aref acc i)
                                     (cons entry (aref acc i))))
                   acc)
                 all-entries
                 :initial-value (make-vector (length (car all-entries)) nil))))
    (unless biome-grid--columns-display
      (setq biome-grid--columns-display (nreverse columns-display)))))

(defun biome-grid-bury-or-kill-this-buffer ()
  "Undisplay the current buffer.

Bury the current buffer, unless there is only one window showing
it, in which case it is killed."
  (interactive)
  (if (> (length (get-buffer-window-list nil nil t)) 1)
      (bury-buffer)
    (kill-buffer)))

(defun biome-grid--update-columns (columns)
  "Update the columns of the grid.

COLUMNS is a list of column names prefixed with \"--\" (to work with
transient switches)."
  (interactive (list (transient-args transient-current-command)))
  (setq biome-grid--columns-display
        (cl-loop for (key name _display) in biome-grid--columns-display
                 collect (list key name (member (format "--%s" key) columns))))
  (biome-grid--set-list biome-grid--current-query biome-grid--current-results)
  (tabulated-list-print t)
  (tabulated-list-init-header))

(transient-define-prefix biome-grid-columns ()
  "Toggle columns in `biome-grid' buffer."
  ["Toggle columns"
   :class transient-column
   :setup-children
   (lambda (_)
     (let ((keys (biome-query--unique-keys
                  (mapcar #'cadr biome-grid--columns-display) '("q"))))
       (transient-parse-suffixes
        'transient-prefix
        (cl-loop for (api-key name display) in biome-grid--columns-display
                 for key = (gethash name keys)
                 collect
                 `(,key ,name ,(format "--%s" api-key)
                        :init-value
                        (lambda (obj)
                          (oset obj value ,(if display (format "--%s" api-key) nil))))))))]
  ["Actions"
   :class transient-row
   ("RET" "Apply" biome-grid--update-columns :transient t)
   ("q" "Quit" transient-quit-one)])

(defun biome-grid-export-csv (file-name query results)
  "Export the displayed `biome' results as CSV.

FILE-NAME is the target file name.  QUERY is a form as defined by
`biome-query-current'.  RESULTS is a response of Open Meteo (returned
by `biome-api-get')."
  (interactive (list (read-file-name "CSV: " nil "result.csv")
                     biome-grid--current-query
                     biome-grid--current-results))
  (let* ((group (intern (alist-get :group query)))
         (group-units (intern (format "%s_units" (alist-get :group query))))
         (var-names (biome-query--get-var-names-cache))
         (data (seq-filter
                (lambda (group)
                  (or (null biome-grid--columns-display)
                      (nth 1 (alist-get (car group) biome-grid--columns-display))))
                (copy-tree (alist-get group results))))
         (data-i (seq-max (mapcar (lambda (datum) (length (cdr datum))) data))))
    (with-temp-file file-name
      (insert (string-join
               (cl-loop for (key . _value) in data
                        for unit = (replace-regexp-in-string
                                    (regexp-quote "%") "%%"
                                    (alist-get key (alist-get group-units results)))
                        for var-name = (biome-query--get-header (symbol-name key) var-names)
                        for col-name = (if (and biome-grid-display-units
                                                (not (string-empty-p unit)))
                                           (format "%s (%s)" var-name unit) var-name)
                        collect (format "\"%s\"" col-name))
               ",")
              "\n"
              (cl-loop for i from 0 to (1- data-i)
                       concat (mapconcat
                               (lambda (datum-item)
                                 (if (length> (cdr datum-item) i)
                                     (let ((item (aref (cdr datum-item) i)))
                                       (cond ((numberp item) (format "%s" item))
                                             ((and (stringp item)
                                                   (string-match-p " " item))
                                              (format "\"%s\"" item))
                                             ((stringp item) (format "%s" item))
                                             (t "")))
                                   ""))
                               data ",")
                       concat "\n")))))


(defvar biome-grid-mode-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap tabulated-list-mode-map)
    (define-key keymap (kbd "q") #'biome-grid-bury-or-kill-this-buffer)
    (define-key keymap (kbd "c") #'biome-grid-columns)
    (define-key keymap (kbd "s") #'biome-grid-export-csv)
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'normal keymap
        "q" #'biome-grid-bury-or-kill-this-buffer
        "c" #'biome-grid-columns
        "s" #'biome-grid-export-csv
        "{" #'tabulated-list-narrow-current-column
        "}" #'tabulated-list-widen-current-column))
    keymap)
  "Keymap for `biome-grid-mode'.")

(defun biome-grid--maybe-highlight-current ()
  "Highlight current hour or day (if hour is not found)."
  (when (and biome-grid-highlight-current
             (length> tabulated-list-entries 1))
    (save-excursion
      (goto-char (point-min))
      (let ((needle-datetime
             (substring
              (format-time-string biome-grid-datetime-format (current-time))
              ;; Remove seconds
              0 -3))
            (needle-date
             (format-time-string biome-grid-date-format (current-time)))
            start end)
        (if (search-forward needle-datetime nil t)
            (setq start (line-beginning-position)
                  end (line-end-position))
          (goto-char (point-min))
          (when (search-forward needle-date nil t)
            (setq start (line-beginning-position)
                  end (line-end-position))))
        (when (and start end)
          (let ((ov (make-overlay start end)))
            (overlay-put ov 'priority -49)
            (overlay-put ov 'face 'biome-grid-highlight-current-face)))))))

(define-derived-mode biome-grid-mode tabulated-list-mode "Biome Grid"
  "Major mode for displaying biome results.

\\{biome-grid-mode-map}")

(defun biome-grid (query results)
  "Display RESULTS in a grid.

QUERY is a form as defined by `biome-query-current'.  RESULTS is a
response of Open Meteo (returned by `biome-api-get'.

The grid is displayed with `tabulated-list'.  It doesn't support
displaying more columns than the window width, so there's
`biome-grid-columns' to toggle columns."
  (let ((buf (generate-new-buffer "*biome-grid*")))
    (with-current-buffer buf
      (let (biome-grid-mode-hook)
        (biome-grid-mode))
      (biome-grid--set-list query results)
      (tabulated-list-print t)
      (tabulated-list-init-header)
      (toggle-truncate-lines 1)
      (biome-grid--maybe-highlight-current)
      (run-mode-hooks 'biome-grid-mode-hook))
    (switch-to-buffer buf)))

(provide 'biome-grid)
;;; biome-grid.el ends here
