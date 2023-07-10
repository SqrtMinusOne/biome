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
(require 'compat)
(require 'transient)

(defcustom biome-query-max-fields-in-row 20
  "Maximum number of fields in a row."
  :type 'integer
  :group 'biome)

(defconst biome-query-groups '("daily" "hourly" "minutely_15" "hourly")
  "Name of groups.

A group is a mutually exclusive choice.  E.g. in the \"Weather
Forecast\" report you can choose between \"daily\" and \"hourly\".  In
principle, the API can return results for both groups, but they would
have to be displayed separately.")

(defconst biome-query--ignore-items '("m" "cm")
  "Items to ignore when generating unique keys.")

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
    (let* ((items (seq-filter
                   (lambda (i)
                     (not (member i biome-query--ignore-items)))
                   (split-string name-low)))
           (sequences (mapcar (lambda (item)
                                (if (string-match-p (rx num) item)
                                    (number-sequence 0 (length item) (length item))
                                  (number-sequence 0 (length item) 1)))
                              items)))
      (dolist (item-take (thread-last
                           (reverse sequences)
                           (reduce #'biome-query--cartesian-product)
                           (mapcar (lambda (it) (if (listp it) (nreverse it) (list it))))
                           (seq-sort-by
                            (lambda (it)
                              ;; TODO better weight function
                              (cl-loop for take in it
                                       for seq in sequences
                                       if (= 2 (length seq)) sum 1
                                       else sum take))
                            #'<)))
        (let ((val (cl-loop for i from 0
                            for item in items
                            for take = (nth i item-take)
                            concat (seq-take item take))))
          (unless (or (gethash val generated-keys)
                      (string-empty-p val)
                      (string-match-p (rx bos (+ num) eos) val))
            (puthash val t generated-keys)
            (iter-yield val)))))))

;; (setq my/test (biome-query--unique-key-cands "Tempe 200"))
;; (iter-next my/test)

(defun biome-query--unique-keys (names)
  "Get unique keys for NAMES.

NAMES is a list of strings."
  (let ((keys-by-name (make-hash-table :test 'equal))
        (names-by-key (make-hash-table :test 'equal))
        (iters (make-hash-table :test 'equal)))
    (cl-loop for name in names
             do (puthash name (biome-query--unique-key-cands name) iters))
    (while-let ((names-to-update
                 (append
                  ;; Unset keys
                  (cl-loop for name in names
                           if (null (gethash name keys-by-name))
                           collect name)
                  ;; Duplicate keys
                  (cl-loop for key being the hash-key of names-by-key
                           using (hash-values names)
                           if (< 1 (length names))
                           collect (car (seq-sort-by #'length #'> names)))
                  ;; Duplicate subkeys
                  (cl-loop for key being the hash-key of names-by-key
                           append (cl-loop
                                   for i from 1 to (1- (length key))
                                   for subkey = (seq-take key i)
                                   for dupe-names = (gethash subkey names-by-key)
                                   when dupe-names append dupe-names)))))
      (cl-loop
       for name in names-to-update
       for old-key = (gethash name keys-by-name)
       for key = (iter-next (gethash name iters))
       if old-key do (puthash old-key (remove name (gethash old-key names-by-key)) names-by-key)
       do (puthash key (cons name (gethash key names-by-key)) names-by-key)
       do (puthash name key keys-by-name)))
    keys-by-name))

(defun biome-query--section-fields-children (fields keys)
  "Get transient laoyut for FIELDS.

FIELDS is a list of fields as defined in `biome-api-parse--page'.
KEYS is the result of `biome-query--unique-keys'."
  (when fields
    `(["Fields"
       :class transient-columns
       ,@(thread-last
           fields
           (seq-map-indexed
            (lambda (field idx) (cons field (/ idx biome-query-max-fields-in-row))))
           (seq-group-by #'cdr)
           (mapcar (lambda (group)
                     (apply #'vector
                            (mapcar
                             (lambda (el)
                               (let* ((field (car el))
                                      (name (alist-get :name (cdr field))))
                                 ;; TODO
                                 (list (gethash name keys) name #'transient-quit-one)))
                             (cdr group))))))])))

(defun biome-query--section-sections-children (sections keys parents)
  "Get transient layout for SECTIONS.

SECTIONS is a list of sections as defined in `biome-api-parse--page'.
KEYS is the result of `biome-query--unique-keys'.  PARENTS is a
list of parent sections."
  (when sections
    `(["Sections"
       :class transient-row
       ,@(mapcar
          (lambda (section)
            `(,(gethash (alist-get :name section) keys)
              ,(alist-get :name section)
              (lambda ()
                (interactive)
                (biome-query--section ',section ',parents))
              :transient transient--do-replace))
          sections)])))

(defun biome-query--section-layout (section parents)
  "Get transient layout for SECTION.

SECTION is a form as defined by `biome-api-parse--page'.  PARENTS
is a list of parent sections."
  (let* ((sections (or (alist-get :children section)
                       (alist-get :sections section)))
         (fields (alist-get :fields section))
         (keys (biome-query--unique-keys
                (append
                 (mapcar (lambda (s) (alist-get :name s)) sections)
                 (mapcar (lambda (s) (alist-get :name (cdr s))) fields)))))
    (append
     (biome-query--section-fields-children fields keys)
     (biome-query--section-sections-children sections keys (cons section parents)))))

(defun biome-query--transient-prepare-layout (name suffixes)
  "Prepare dynamic transient layout for NAME.

SUFFIXES is a list of suffix definitions."
  (thread-last
    suffixes
    (cl-mapcan (lambda (s) (transient--parse-child 'tsc-dynamic-layout s)))
    (mapcar #'eval)
    (put name 'transient--layout)))

(transient-define-prefix biome-query--section (section &optional parents)
  "Render transient for SECTION.

SECTION is a form as defined in `biome-api-parse--page'."
  (interactive (list nil))
  (unwind-protect
      (progn
        (setq my/test section)
        (biome-query--prepare-layout
         'biome-query--section
         (append
          '([:description
             (lambda () (alist-get :name (car (oref transient--prefix scope))))
             (biome-query--transient-report-infix)])
          (biome-query--section-layout section parents)
          '(["Actions"
             :class transient-row
             ("q" "Up" transient-quit-one)
             ("Q" "Quit" transient-quit-all)])))
        (transient-setup 'biome-query--section nil nil :scope
                         `((:section . ,section)
                           (:parents . ,parents))))
    (put 'biome-query-section  'transient--layout nil)))

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
                           (biome-query--section ',params))
                         :transient transient--do-replace))))]
  ["Actions"
   ("q" "Quit" transient-quit-one)])

(provide 'biome-query)
;;; biome-query.el ends here
