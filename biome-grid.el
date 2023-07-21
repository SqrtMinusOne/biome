;;; biome-grid.el --- Display results of open meteo queries -*- lexical-binding: t -*-

;; Copyright (C) 2023 Korytov Pavel

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

;; TODO

;;; Code:
(require 'cl-lib)
(require 'ct)
(require 'seq)
(require 'tabulated-list)
(require 'biome-query)

(defcustom biome-grid-display-units t
  "Display units in the grid."
  :type 'boolean
  :group 'biome)

(defcustom biome-grid-format-units
  '(("°C" . (gradient
             (-40 . "#ae64a0")
             (-30 . "#9488d2")
             (-15 . "#90cfd2")
             (-5 . "#66adbb")
             (5 . "#508c40")
             (15 . "#aba10e")
             (25 . "#f39506")
             (35 . "#bf4112")
             (40 . "#8a2b0a"))))
  "Format units in the grid."
  :group 'biome)

(defvar biome-grid-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") #'quit-window)
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'normal keymap
        "q" #'quit-window))
    keymap)
  "Keymap for `biome-api-error-mode'.")

(define-derived-mode biome-grid-mode tabulated-list-mode "Biome Grid"
  "Major mode for displaying biome results.")

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

(defun biome-grid--format-entries (entries unit col-width)
  (let ((format-def (alist-get unit biome-grid-format-units nil nil #'equal)))
    (mapcar
     (lambda (entry)
       (cond
        ((stringp entry) entry)
        ((eq (car-safe format-def) 'gradient)
         (biome-grid--format-gradient entry (cdr format-def) col-width))
        (t (prin1-to-string entry))))
     entries)))

(defun biome-grid--set-list (query results)
  (let* ((group (intern (alist-get :group query)))
         (group-units (intern (format "%s_units" (alist-get :group query))))
         (var-names (biome-query--get-var-names-cache))
         all-entries columns)
    (cl-loop for (key . values) in (alist-get group results)
             for unit = (replace-regexp-in-string
                         (regexp-quote "%") "%%"
                         (alist-get key (alist-get group-units results)))
             for var-name = (biome-query--get-header (symbol-name key) var-names)
             for col-name = (if biome-grid-display-units
                                (format "%s (%s)" var-name unit)
                              var-name)
             for col-width = (cl-loop for entry in (cons col-name entries)
                                      maximize (+ 0 (length entry)))
             for entries = (biome-grid--format-entries values unit col-width)
             do (push (list col-name col-width nil) columns)
             do (push entries all-entries))
    (setq-local
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
                 :initial-value (make-vector (length (car all-entries)) nil))))))

(defun biome-grid (query results)
  "Display RESULTS in a grid."
  (setq my/test results)
  (let ((buf (generate-new-buffer "*biome-grid*")))
    (with-current-buffer buf
      (biome-grid--set-list query results)
      (biome-grid-mode)
      (toggle-truncate-lines 1))
    (switch-to-buffer buf)))

(provide 'biome-grid)
;;; biome-grid.el ends here
