;;; biome.el --- Bountiful Interface to Open Meteo for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
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
  "Run multiple queries to Open Meteo and join results."
  (interactive)
  (biome-multi-query
   (lambda (query)
     (biome-api-get-multiple
      query
      (lambda (queries results)
        (let ((merged (biome-multi--merge queries results)))
          (funcall biome-frontend (nth 0 merged) (nth 1 merged))))))))

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

(provide 'biome)
;;; biome.el ends here
