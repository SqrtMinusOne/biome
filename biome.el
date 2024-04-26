;;; biome.el --- Bountiful Interface to Open Meteo for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1") (transient "0.3.7") (ct "0.2") (request "0.3.3") (compat "29.1.4.1"))
;; Homepage: https://github.com/SqrtMinusOne/biome
;; Published-At: 2023-07-22

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

;; Interface to Open Meteo <https://open-meteo.com> for Emacs.  The
;; API provides weather forecasts, historical weather data, climate
;; change projections, and more.
;;
;; The API is free for non-commercial use if you make less than 10000
;; requests per day.
;;
;; The package has two main entrypoints: `biome' and `biome-resume'.  The
;; former starts a new query, the latter resumes the last query.
;;
;; Also check out the README at <https://github.com/SqrtMinusOne/biome>.

;;; Code:
(require 'biome-api)
(require 'biome-multi)
(require 'biome-query)
(require 'biome-grid)

(defgroup biome nil
  "Bountiful Interface to Open Meteo for Emacs."
  :group 'applications)

(defcustom biome-frontend #'biome-grid
  "The frontend to use for displaying the data.

This has to be a function that receives two arguments: query (as
defined by `biome-query-current' and the response of the open-meteo
API."
  :type 'function
  :group 'biome)

(defcustom biome-presets-alist nil
  "Presets for `biome' queries.

One item of the list is another list with three elements:
- preset name;
- `:normal' for normal `biome' or `:multi' for `biome-multi';
- parameters as defined by `biome-query-current' or
  `biome-multi-query-current'.
Thus, preset names are the keys of the alist.

To generate expressions that add stuff to this list, run \"Generate
preset definition\" in `biome' or `biome-multi'."
  :type '(repeat
          (list
           (string :tag "Preset name")
           (choice (const :tag "Normal" :normal)
                   (const :tag "Multi" :multi))
           (sexp :tag "Parameters")))
  :group 'biome)

(defun biome ()
  "Bountiful Interface to Open Meteo for Emacs."
  (interactive)
  (funcall-interactively
   #'biome-query
   (lambda (query)
     (biome-api-get query biome-frontend))))

(defun biome-resume ()
  "Resume the last query."
  (interactive)
  (unless biome-query-current
    (user-error "Nothing to resume"))
  ;; `biome-query--callback' should have been already set by the
  ;; previous invocation of `biome'
  (biome-query--section-open (alist-get :name biome-query-current)))

(defun biome-multi ()
  "Run multiple queries to Open Meteo and join the results."
  (interactive)
  (funcall-interactively
   #'biome-multi-query
   (lambda (query)
     (biome-api-get-multiple
      query
      (lambda (queries results)
        (let ((merged (biome-multi--merge queries results)))
          (funcall biome-frontend (nth 0 merged) (nth 1 merged))))))))

(defun biome-multi-history ()
  "Get historical weather data on a particular day."
  (interactive)
  (funcall-interactively
   #'biome-multi--history-query
   (lambda (queries)
     (biome-api-get-multiple
      queries
      (lambda (queries results)
        (let ((concat-results (biome-multi--concat-results queries results)))
          (funcall biome-frontend (car queries) concat-results)))))))

(defmacro biome-def-preset (name params)
  "Declare a query preset.

NAME is the name of the target function.  PARAMS is a form as defined
by `biome-query-current'.

This macro creates an interactive function that runs `biome'with
PARAMS as query."
  (declare (indent 1))
  `(defun ,name ()
     (interactive)
     (setq biome-query--callback
           (lambda (query)
             (biome-api-get query biome-frontend)))
     (setq biome-query-current ',params)
     (biome-query--section-open (alist-get :name ',params))))

(defmacro biome-def-multi-preset (name params)
  "Declare a multi-query preset.

NAME is the name of the target function.  PARAMS is a form as defined
by `biome-multi-query-current'.

This macro creates an interactive function that runs `biome-multi' with
PARAMS as query."
  (declare (indent 1))
  `(defun ,name ()
     (interactive)
     (setq biome-multi-query-current ',params)
     (call-interactively #'biome-multi)))

(defun biome-preset (preset-def)
  "Run `biome' with a preset.

PRESET-DEF is one preset as defined by `biome-presets-alist', sans the
name.  If run interactively, prompt PRESET-DEF from
`biome-presets-alist'.

Run \"Generate preset definition\" in `biome' or `biome-multi' to
generate expressions that add stuff to `biome-presets-alist'."
  (interactive (list (alist-get
                      (completing-read "Preset" biome-presets-alist)
                      biome-presets-alist nil nil #'equal)))
  (pcase-let ((`(,kind ,params) preset-def))
    (pcase kind
      (:normal
       (setq biome-query--callback
             (lambda (query)
               (biome-api-get query biome-frontend)))
       (setq biome-query-current (copy-tree params))
       (biome-query--section-open (alist-get :name params)))
      (:multi
       (setq biome-multi-query-current (copy-tree params))
       (call-interactively #'biome-multi)))))

(provide 'biome)
;;; biome.el ends here
