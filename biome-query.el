;;; biome-query.el --- Query interface for Open Meteo -*- lexical-binding: t -*-

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
(require 'biome-api-data)
(require 'transient)

(defconst biome-query-groups '("daily" "hourly" "minutely_15" "hourly")
  "Name of groups.

A group is a mutually exclusive choice.  E.g. in the \"Weather
Forecast\" report you can choose between \"daily\" and \"hourly\".  In
principle, the API can return results for both groups, but they would
have to be displayed separately.")

(defvar biome-query-current nil
  "Current report.

It is an alist with the following keys:
- `:name' - name of the root section.
- `:kind' - name of the group (see `biome-query-groups').
- `:parameters' - alist with parameters, where the key is either nil
  (for global parameters) or the value of `:param' key of the
  corresponding section.
  In the former case, the value is an alist with values; in the latter
  case, the value is a list of variable names available in the group.")

(defclass biome-query--transient-report (transient-suffix)
  ((transient :initform t))
  "A transient class to display the current report.")

(cl-defmethod transient-init-value ((_ biome-query--transient-report))
  "A dummy method for `biome-query--transient-report'."
  nil)

(cl-defmethod transient-format ((_ biome-query--transient-report))
  "Format the current report."
  (prin1-to-string biome-query-current))

(transient-define-infix biome-query--transient-report-infix ()
  :class 'biome-query--transient-report
  :key "~~1")

(defun biome-query--cartesian-product (a b)
  "Compute the Cartesian product of A and B."
  (mapcan
   (lambda (item-from-a)
     (mapcar
      (lambda (item-from-b)
        (if (listp item-from-a)
            (append item-from-a (list item-from-b))
          (list item-from-a item-from-b)))
      b))
   a))

(iter-defun biome-query--unique-key-cands (name)
  "Generate unique key candidates for NAME."
  (let ((name-low (replace-regexp-in-string (rx (not alnum))  " " (downcase name)))
        (generated-keys (make-hash-table :test 'equal)))
    (let* ((items (split-string name-low))
           (sequences (mapcar (lambda (item)
                                (if (string-match-p (rx num) item)
                                    (number-sequence 0 (length item) (length item))
                                  (number-sequence 0 (length item) 1)))
                              items)))
      (dolist (item-take (reduce #'biome-query--cartesian-product (nreverse sequences)))
        (let ((val (cl-loop for i from 0
                            for item in items
                            for take = (nth (- (1- (length items)) i) item-take)
                            concat (seq-take item take))))
          (unless (or (gethash val generated-keys)
                      (string-empty-p val))
            (puthash val t generated-keys)
            (iter-yield val)))))))

(defun biome-query--unique-keys (name keys)
  "Get a unique key for NAME.

NAME is a field name.  KEYS is a hash-map of used key names."
  (seq-some ))

(defun biome-query--section-children (section)
  (let ((sections (alist-get :sections section))
        (fields (alist-get :fields section))
        (keys (make-hash-table :test 'equal))
        res)
    (cl-loop for section in sections
             do (puthash (alist-get :name section) section keys))
    ))

(transient-define-prefix biome-query--root-section (section)
  "Render transient for root SECTION.

SECTION is a form as defined in `biome-api-parse--page'."
  [:description
   (lambda () (alist-get :name (oref transient--prefix scope)))
   (biome-query--transient-report-infix)]
  ["Actions"
   :class transient-row
   ("q" "Up" transient-quit-one)
   ("Q" "Quit" transient-quit-all)]
  (interactive (list nil))
  (transient-setup 'biome-query--section nil nil :scope section))

(transient-define-prefix biome-query ()
  ["Open Meteo Data"
   :setup-children
   (lambda (_)
     (cl-loop for (name . params) in biome-api-data
              collect (transient-parse-suffix
                       transient--prefix
                       `(,(alist-get :key params)
                         ,name
                         (lambda () (interactive)
                           (when (and biome-query-current
                                      (not (equal ,name (alist-get :name biome-query-current))))
                             (setq biome-query-current nil))
                           (unless biome-query-current
                             (setq biome-query-current
                                   '((:name . ,name)
                                     (:kind . nil)
                                     (:parameters . nil))))
                           (biome-query--root-section ',params))
                         :transient transient--do-replace))))]
  ["Actions"
   ("q" "Quit" transient-quit-one)])

(provide 'biome-query)
;;; biome-query.el ends here
