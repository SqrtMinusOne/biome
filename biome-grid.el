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
(require 'biome-query)
(require 'tabulated-list)

(defcustom biome-grid-display-units t
  "Display units in the grid."
  :type 'boolean
  :group 'biome)

(define-derived-mode biome-grid-mode tabulated-list-mode "Biome Grid"
  "Major mode for displaying biome results."
  (setq-local truncate-lines t))

(defun biome-grid--format-entries (entries unit)
  (mapcar
   (lambda (entry)
     (if (stringp entry)
         entry
       (prin1-to-string entry)))
   entries))

(defun biome-grid--get-width (entries col-name)
  (cl-loop for entry in (cons col-name entries)
           maximize (length entry)))

(defun biome-grid--set-list (query results)
  (let* ((group (intern (alist-get :group query)))
         (group-units (intern (format "%s_units" (alist-get :group query))))
         (var-names (biome-query--get-var-names-cache))
         all-entries columns)
    (cl-loop for (key . values) in (alist-get group results)
             for unit = (alist-get key (alist-get group-units results))
             for var-name = (biome-query--get-header (symbol-name key) var-names)
             for col-name = (if biome-grid-display-units
                                (format "%s (%s)" var-name unit)
                              var-name)
             for entries = (biome-grid--format-entries values unit)
             for col-width = (biome-grid--get-width entries col-name)
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
  (let ((buf (generate-new-buffer "*biome-grid*")))
    (with-current-buffer buf
      (biome-grid--set-list query results)
      (biome-grid-mode))
    (display-buffer buf #'display-buffer-same-window)))

(provide 'biome-grid)
;;; biome-grid.el ends here
