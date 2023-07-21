;;; biome.el --- Bountiful Interface to Open Meteo for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (transient "0.3.7") (ct "0.2") (request "0.3.3") (compat "29.1.4.1"))
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

;;  TODO

;;; Code:
(require 'biome-api)
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
  (biome-query
   (lambda (query)
     (biome-api-get query biome-frontend))))

(provide 'biome)
;;; biome.el ends here
