;;; biome-query.el --- Query interface for Open Meteo -*- lexical-binding: t -*-

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

;; Query editor for Open Meteo API.
;;
;; The main entrypoints are:
;; - `biome-query' - start a new query.
;; - `biome-query--section-open' - start a new query in a section.
;; These aren't meant to be used directly by the user.

;;; Code:
(require 'compat)
(require 'cl-lib)
(require 'font-lock)
(require 'generator)
(require 'org)
(require 'transient)

(require 'biome-api-data)

;; XXX Recursive imports T_T
(declare-function biome-preset "biome")
(declare-function biome-multi "biome-multi")
(declare-function biome-multi-history "biome-multi")

(defcustom biome-query-max-fields-in-row 20
  "Maximum number of fields in a row."
  :type 'integer
  :group 'biome)

(defcustom biome-query-completing-read-threshold 6
  "Invoke `completing-read' when there are more than this many choices."
  :type 'integer
  :group 'biome)

(defcustom biome-query-date-format "%A, %x"
  "Format string for date entries in query buffers.

By default \"WEEKDAY, DATE\", where DATE is what Emacs thinks is an
appropriate way to format days in your language.
If the value is a function, the function will be evaluated and the return
value will be inserted."
  :type '(choice
          (string :tag "String")
          (function :tag "Function"))
  :group 'biome)

(defcustom biome-query-coords '(("Helsinki, Finland" 60.16952 24.93545)
                                ("Berlin, Germany" 52.52437 13.41053)
                                ("Dubai, UAE" 25.0657 55.17128))
  "List of saved locations with their coordinates.

The format is: (name latitude longitude)."
  :type '(repeat (list
                  (string :tag "Name")
                  (number :tag "Latitude")
                  (number :tag "Longitude")))
  :group 'biome)

(defcustom biome-query-override-column-names nil
  "Override column names for variables."
  :type '(repeat (cons
                  (string :tag "Variable name")
                  (string :tag "Column name")))
  :group 'biome)

(defcustom biome-query-tab-key "<tab>"
  "Key used for TAB."
  :type 'string
  :group 'biome)

(defconst biome-query--max-sections-for-row 6
  "Maximum number of sections to use for `transient-row'.")

(defconst biome-query-groups '("daily" "hourly" "minutely_15" "hourly" "current")
  "Name of groups.

A group is a mutually exclusive choice.  E.g. in the \"Weather
Forecast\" report you can choose between \"daily\" and \"hourly\".  In
principle, the API can return results for both groups, but they would
have to be displayed separately.")

(defconst biome-query--split-items '(("timezone" . "time zone")
                                     ("timeformat" . "time format")
                                     ("weathercode" . "weather code")
                                     ("iso8601" . "iso 8")
                                     ;; I'm used to "c" for "coordinates"
                                     ("current weather" . "urrent weather"))
  "Items to split into separate words for generating keys.")

(defconst biome-query--ignore-items '("m" "cm")
  "Items to ignore when generating unique keys.")

(defvar biome-query-current nil
  "Current query.

It is an alist with the following keys:
- `:name' - name of the root section.
- `:group' - name of the group (see `biome-query-groups').
- `:params' - alist with parameters, where the key is either the
  parameter name (for global variables) or the group name (for grouped
  variables, e.g. hourly, daily).  In the latter case, the value has
  to be a list of grouped variables names.")

(defvar biome-query--current-section nil
  "Current section definition.")

(defvar biome-query--layout-cache (make-hash-table :test 'equal)
  "Cache for dynamic transient layout.")

(defvar biome-query--var-names-cache nil
  "Cache for variable names.")

(defvar biome-query--callback nil
  "Call this with the selected query.")

;; Transient display classes
(defclass biome-query--transient-report (transient-suffix)
  ((transient :initform t))
  "A transient class to display the current report.

This just prints out `biome-query-current' in somewhat readable
form.")

(cl-defmethod transient-init-value ((_ biome-query--transient-report))
  "A dummy method for `biome-query--transient-report'."
  nil)

(defun biome-query--update-names-cache (sections cache)
  "Update the variable names cache for SECTIONS.

CACHE is a hash table.  See `biome-query--get-var-names-cache'."
  (cl-loop for section in sections
           do (when-let (fields (alist-get :fields section))
                (cl-loop for (api-key . params) in fields
                         do (puthash api-key (alist-get :name params) cache)))
           do (when-let (children (or (alist-get :sections section)
                                      (alist-get :children section)))
                (biome-query--update-names-cache children cache))))

(defun biome-query--get-var-names-cache ()
  "Generate variable names cache for the current section.

The current section is determined by `biome-query-current'.  The
return value is a hashmap with variable api keys as keys and their
names as values.

This is useful because otherwise it would take a long time to look for
disambiguations of parameters in `biome-api-data'."
  (let* ((name (alist-get :name biome-query-current))
         (cache (alist-get name biome-query--var-names-cache
                           nil nil #'equal)))
    (if (and (hash-table-p cache) (> (hash-table-count cache) 0)) cache
      (setq cache (make-hash-table :test #'equal))
      (biome-query--update-names-cache
       (alist-get :sections biome-query--current-section)
       cache)
      (setf (alist-get name biome-query--var-names-cache
                       nil nil #'equal)
            cache))))

(defun biome-query--get-header (key var-names)
  "Generate readable name for KEY with a fallback.

KEY is the api key of the variable.  VAR-NAMES is the output of
`biome-query--get-var-names-cache'."
  (or (cdr (assoc key biome-query-override-column-names))
      (gethash key var-names)
      (capitalize (replace-regexp-in-string
                   (regexp-quote "_") " " key))))

(defun biome-query--format (query)
  "Format QUERY for display.

QUERY is a form as defined by `transient-define-prefix'."
  (let ((group (alist-get :group query))
        (var-names (biome-query--get-var-names-cache))
        lat lon group-vars line-vars vars)
    (dolist (item (alist-get :params query))
      (cond
       ((stringp item)
        (push (biome-query--get-header item var-names) vars))
       ((equal (car item) group)
        (setq group-vars
              (mapcar (lambda (x)
                        (biome-query--get-header x var-names))
                      (cdr item))))
       ((equal (car item) "latitude")
        (setq lat (cdr item)))
       ((equal (car item) "longitude")
        (setq lon (cdr item)))
       ((member (car item) '("end_date" "start_date" "day_of_year"))
        (push
         (format "%s: %s" (propertize
                           (biome-query--get-header (car item) var-names)
                           'face 'font-lock-variable-name-face)
                 (propertize
                  (format-time-string biome-query-date-format (cdr item))
                  'face 'transient-value-face))
         vars))
       ((listp (cdr item))
        (push
         (format "%s: %s"
                 (biome-query--get-header (car item) var-names)
                 (propertize
                  (mapconcat #'identity (cdr item) "; ")
                  'face 'font-lock-variable-name-face))
         line-vars))
       (t (push
           (format "%s: %s"
                   (propertize
                    (biome-query--get-header (car item) var-names)
                    'face 'font-lock-variable-name-face)
                   (propertize
                    (prin1-to-string (cdr item))
                    'face 'transient-value-face))
           vars))))
    (setq group-vars (nreverse group-vars)
          line-vars (nreverse line-vars)
          vars (nreverse vars))
    (concat "Location: "
            (if lat (propertize (number-to-string lat) 'face 'transient-value)
              (propertize "unset" 'face 'error))
            " "
            (if lon (propertize (number-to-string lon) 'face 'transient-value)
              (propertize "unset" 'face 'error))
            ;; XXX byte-compiler thinks the variable `nothing' is
            ;; used, so I can't call it "_"
            (when-let ((nothing (and lat lon))
                       (loc (seq-find
                             (lambda (x) (equal (cdr x) (list lat lon)))
                             biome-query-coords)))
              (format " (%s)" (propertize (car loc) 'face 'transient-value)))
            "\n"
            (when group
              (format "Group: %s\n"
                      (propertize group 'face 'transient-value)))
            (when (or group group-vars)
              (format "Group variables: %s\n"
                      (if group-vars
                          (mapconcat (lambda (x) (propertize x 'face 'font-lock-variable-name-face))
                                     group-vars "; ")
                        (propertize "unset" 'face 'error))))
            (when vars
              (format "Variables: %s\n"
                      (mapconcat #'identity vars "; ")))
            (when line-vars
              (concat (mapconcat #'identity line-vars "\n") "\n")))))

(cl-defmethod transient-format ((_obj biome-query--transient-report))
  "Format the `biome-query-current'."
  (biome-query--format biome-query-current))

(transient-define-infix biome-query--transient-report-infix ()
  :class 'biome-query--transient-report
  :key "~~1")

(defclass biome-query--transient-path (transient-suffix)
  ((transient :initform t))
  "A transient class to display the current path in query.")

(cl-defmethod transient-init-value ((_ biome-query--transient-path))
  "A dummy method for `biome-query--transient-report'."
  nil)

(cl-defmethod transient-format ((_ biome-query--transient-path))
  "Format the current path in query."
  (let* ((scope (oref (or transient--prefix
                          transient-current-prefix)
                      scope))
         (parents (alist-get :parents scope))
         (section (alist-get :section scope)))
    (concat
     (cl-loop for parent in (reverse parents)
              concat (propertize
                      (alist-get :name parent)
                      'face 'font-lock-variable-name-face)
              concat " > ")
     (propertize
      (alist-get :name section)
      'face 'transient-heading))))

(transient-define-infix biome-query--transient-path-infix ()
  :class 'biome-query--transient-path
  :key "~~1")

(defclass biome-query--transient-switch-variable (transient-argument)
  ((api-key :initarg :api-key)
   (param :initarg :param))
  "A transient class to display a switch.

The switch works the following way: if `:param' is non-nil, then the
value corresponds to API_KEY being in the list of values in
`biome-query-current' for `:param'.  Cases when `:param' is nil
shouldn't exist.")

(cl-defmethod transient-init-value ((obj biome-query--transient-switch-variable))
  "Initialize the value of the variable switch.

OBJ is an instance of `biome-query--transient-switch-variable'."
  (oset obj value
        (and
         (member
          (oref obj api-key)
          (if-let ((param (oref obj param)))
              (cdr
               (assoc
                param
                (alist-get :params biome-query-current)))
            (alist-get :params biome-query-current)))
         t)))

(defmacro biome-query--update-list (item list-place add)
  "Add or remove ITEM from LIST-PLACE depending on ADD."
  `(setf ,list-place
         (if ,add
             (cons ,item ,list-place)
           (delete ,item ,list-place))))

(cl-defmethod transient-infix-read ((obj biome-query--transient-switch-variable))
  "Toggle the variable switch on or off.

OBJ is an instance of `biome-query--transient-switch-variable'."
  (let ((new-value (not (oref obj value)))
        (param (oref obj param))
        (api-key (oref obj api-key)))
    (if param
        (biome-query--update-list
         api-key (alist-get param (alist-get :params biome-query-current) nil nil #'equal)
         new-value)
      (biome-query--update-list
       api-key (alist-get :params biome-query-current  nil nil #'equal)
       new-value))
    new-value))

(cl-defmethod transient-format ((obj biome-query--transient-switch-variable))
  "Format the variable switch OBJ."
  (concat
   " "
   (compat-call string-pad (transient-format-key obj) 6)
   (transient-format-description obj)
   (when (oref obj value)
     (propertize " (+)" 'face 'transient-argument))))

(defclass biome-query--transient-variable (transient-variable)
  ((api-key :initarg :api-key))
  "A transient class to work with query variables.

This is mostly used to interact with `biome-query-current'.")

(cl-defmethod transient-infix-set ((obj biome-query--transient-variable) value)
  "Set the value of OBJ to VALUE.

This also updates `biome-query-current' with the new value."
  (if value
      (setf
       (alist-get (oref obj api-key) (alist-get :params biome-query-current)
                  nil nil #'equal)
       value)
    (setf
     (alist-get :params biome-query-current)
     (delq (assoc (oref obj api-key) (alist-get :params biome-query-current))
           (alist-get :params biome-query-current))))
  (oset obj value value))

(cl-defmethod transient-init-value ((obj biome-query--transient-variable))
  "Initialize the value.

OBJ is an instance of `biome-query--transient-select-variable'."
  (oset obj value
        (alist-get (oref obj api-key)
                   (alist-get :params biome-query-current)
                   nil nil #'equal)))

(cl-defmethod transient-format-value ((obj biome-query--transient-variable))
  "Format the value of OBJ."
  (let ((value (if (slot-boundp obj 'value) (slot-value obj 'value) nil)))
    (if value
        (propertize
         (format "%s" value)
         'face 'transient-value)
      (propertize "unset" 'face 'transient-inactive-value))))

(defclass biome-query--transient-date-variable (biome-query--transient-variable) ()
  "A transient class to work with a date variable.

The variable is read via `org-read-date' and stored as a UNIX
timestamp.")

(cl-defmethod transient-infix-read ((obj biome-query--transient-date-variable))
  "Read a date from the user with `org-read-date'.

OBJ is an instance of `biome-query--transient-date-variable'."
  (unless (oref obj value)
    (let ((org-read-date-force-compatible-dates nil))
      (time-convert
       (org-read-date nil t nil (concat (oref obj description) " "))
       'integer))))

(cl-defmethod transient-format-value ((obj biome-query--transient-date-variable))
  "Format the value of OBJ.

OBJ is an instance of `biome-query--transient-date-variable'."
  (let ((value (if (slot-boundp obj 'value) (slot-value obj 'value) nil)))
    (if value
        (propertize
         (format-time-string
          biome-query-date-format
          (seconds-to-time
           value))
         'face 'transient-value)
      (propertize "unset" 'face 'transient-inactive-value))))

(defclass biome-query--transient-select-variable (biome-query--transient-variable)
  ((options :initarg :options))
  "A transient class to display a select variable.

`:options' is an alist of the form ((KEY . DESCRIPTION) ...).  If the
number of options is more than
`biome-query-completing-read-threshold', the user is prompted via
`completing-read'.  Otherwise, it just switches to the next option.")

(cl-defmethod transient-infix-value ((obj biome-query--transient-select-variable))
  "Return the value of OBJ."
  (oref obj value))

(cl-defmethod transient-format-value ((obj biome-query--transient-select-variable))
  "Format the value of OBJ."
  (let ((value (transient-infix-value obj)))
    (concat
     (propertize "[" 'face 'transient-inactive-value)
     (mapconcat
      (lambda (choice)
        (propertize (cdr choice) 'face
                    (if (equal (car choice) value)
                        'transient-value
                      'transient-inactive-value)))
      (oref obj options)
      (propertize "|" 'face 'transient-inactive-value))
     (propertize "]" 'face 'transient-inactive-value))))

(cl-defmethod transient-infix-read ((obj biome-query--transient-select-variable))
  "Read the value of OBJ, either with `completing-read' or by toggle."
  (let* ((options (mapcar
                   (lambda (c) (cons (cdr c) (car c)))
                   (append (oref obj options) (list (cons nil "unset")))))
         (current-idx (or (cl-position (transient-infix-value obj) options
                                       :test (lambda (a b) (equal a (cdr b))))
                          -1))
         (next-idx (% (1+ current-idx) (length options)))
         (value
          (if (> (length options) biome-query-completing-read-threshold)
              (let* ((choice (completing-read
                              (oref obj description)
                              options nil t))
                     (new-value (cdr (assoc choice options))))
                (when (and (null new-value) (not (equal choice "unset")))
                  (user-error "Invalid choice: %s" choice))
                new-value)
            (cdr (nth next-idx options)))))
    value))

(defclass biome-query--transient-number-variable (biome-query--transient-variable)
  ((min :initarg :min :initform nil)
   (max :initarg :max :initform nil)
   (integer :initarg :integer :initform nil))
  "A transient class to work with a number variable.")

(cl-defmethod transient-infix-read ((obj biome-query--transient-number-variable))
  "Read the value of OBJ."
  (let ((prompt
         (concat
          (oref obj description)
          " [Enter "
          (if (oref obj integer) "integer" "number")
          (when (and (oref obj min) (oref obj max))
            (format " between %s and %s" (oref obj min) (oref obj max)))
          (when (and (oref obj min) (not (oref obj max)))
            (format " greater than %s" (oref obj min)))
          (when (and (not (oref obj min)) (oref obj max))
            (format " less than %s" (oref obj max)))
          "]: "))
        ok value)
    ;; XXX `while' doesn't work well with transient
    (let ((res (read-from-minibuffer
                prompt
                (if (numberp (oref obj value))
                    (number-to-string (oref obj value))
                  (oref obj value)))))
      (if (string-empty-p res)
          (setq ok t)
        (if (oref obj integer)
            (setq ok (string-match-p (rx bos (+ digit) eos) res))
          (setq ok (string-match-p (rx bos (+ (or digit ".")) eos) res))
          (when ok
            (setq value (string-to-number res))
            (when (and (oref obj min) (< value (oref obj min)))
              (setq ok nil))
            (when (and (oref obj max) (> value (oref obj max)))
              (setq ok nil)))))
      (if ok value
        (message "Invalid input") nil))))

(cl-defmethod transient-format-value ((obj biome-query--transient-number-variable))
  "Format the value of OBJ."
  (let* ((api-key (oref obj api-key))
         ;; XXX because lat and lon can be updated outside of the
         ;; transient value...  I don't want to use it for every
         ;; variable because that would result in a spam of
         ;; `alist-get', which is slower than `oref'.
         (value (pcase api-key
                  ((or "latitude" "longitude")
                   (alist-get
                    api-key
                    (alist-get :params biome-query-current)
                    nil nil #'equal))
                  (_ (if (slot-boundp obj 'value) (slot-value obj 'value) nil)))))
    (if value
        (propertize
         (number-to-string value)
         'face 'transient-value)
      (propertize "unset" 'face 'transient-inactive-value))))

(defclass biome-query--transient-timezone-variable (biome-query--transient-variable) ()
  "A transient class to work with a timezone variable.")

(cl-defmethod transient-infix-read ((obj biome-query--transient-timezone-variable))
  "Read the value of OBJ."
  (completing-read (concat (oref obj description) " ") biome-api-timezones
                   nil t (oref obj value)))

(defclass biome-query--transient-coords (biome-query--transient-variable) ()
  "A transient class for a coordinate switcher.

The source of possible coordinates is `biome-query-coords'.")

(cl-defmethod transient-init-value ((obj biome-query--transient-coords))
  "Initialize the value of OBJ."
  (oset obj value
        (when-let ((lat (alist-get "latitude" (alist-get :params biome-query-current)
                                   nil nil #'equal))
                   (lon (alist-get "longitude" (alist-get :params biome-query-current)
                                   nil nil #'equal)))
          (seq-find
           (lambda (c) (and (= lat (nth 1 c)) (= lon (nth 2 c))))
           biome-query-coords))))

(cl-defmethod transient-infix-read ((_ biome-query--transient-coords))
  "Read the value of OBJ."
  (assoc (completing-read "Select a location" biome-query-coords nil t)
         biome-query-coords))

(cl-defmethod transient-infix-set ((obj biome-query--transient-coords) value)
  "Set the value of OBJ to VALUE."
  (let ((lat (nth 1 value))
        (lon (nth 2 value)))
    (oset obj value value)
    (setf (alist-get "latitude" (alist-get :params biome-query-current)
                     nil nil #'equal)
          lat
          (alist-get "longitude" (alist-get :params biome-query-current)
                     nil nil #'equal)
          lon)))

(cl-defmethod transient-format-value ((obj biome-query--transient-coords))
  "Format the value of OBJ."
  (if-let ((val (oref obj value))
           (lat (alist-get "latitude" (alist-get :params biome-query-current)
                           nil nil #'equal))
           (lon (alist-get "longitude" (alist-get :params biome-query-current)
                           nil nil #'equal)))
      (if (and (= lat (nth 1 val)) (= lon (nth 2 val)))
          (propertize (car val) 'face 'transient-value)
        (propertize "changed" 'face 'transient-inactive-value))
    (propertize "unset" 'face 'transient-inactive-value)))

(transient-define-infix biome-query--transient-coords-infix ()
  :class 'biome-query--transient-coords
  :key "c"
  :description "Choose location")

(defclass biome-query--transient-group-switch (biome-query--transient-select-variable)
  ((options :initform nil))
  "A transient class to switch between groups of a query.")

(cl-defmethod transient-infix-value ((obj biome-query--transient-group-switch))
  "Return the value of OBJ."
  (oref obj value))

(cl-defmethod transient-init-value ((obj biome-query--transient-group-switch))
  "Initialize the value of OBJ."
  (let ((groups (biome-query--section-groups biome-query--current-section)))
    (oset obj options groups)
    (oset obj value (alist-get :group biome-query-current))))

(cl-defmethod transient-infix-read ((obj biome-query--transient-group-switch))
  "Switch to the next group.

OBJ is an instance of `biome-query--transient-group-switch'."
  (let* ((options (mapcar
                   (lambda (c) (cons (cdr c) (car c)))
                   (oref obj options)))
         (current-idx (cl-position
                       (oref obj value) options
                       :test (lambda (a b) (equal a (cdr b)))))
         (next-idx (% (1+ current-idx) (length options)))
         (value (cdr (nth next-idx options))))
    value))

(cl-defmethod transient-infix-set ((obj biome-query--transient-group-switch) value)
  "Set the value of OBJ to VALUE."
  (let ((old-value (alist-get :group biome-query-current)))
    (setf (alist-get :group biome-query-current) value
          (alist-get :params biome-query-current)
          (seq-filter
           (lambda (elem) (not (equal (car-safe elem) old-value)))
           (alist-get :params biome-query-current)))
    (setf (oref obj value) value)))

(transient-define-infix biome-query--transient-group-switch-infix ()
  :class 'biome-query--transient-group-switch
  :key biome-query-tab-key
  :description "Switch group")

;; Layout generation

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

(defun biome-query--unique-key-weight (it seq-lengths)
  "Compute the weight of a unique key candidate IT.

IT is a list of numbers, where each number means to take this
number of symbols from the start of the corresponding word.
SEQ-LENGTHS is a list of possible values of IT - if it's 2, then
the item is a number that can only be taken as a whole, otherwise
it's the length of the word."
  ;; TODO better weight function?
  (cl-loop for take in it
           for length in seq-lengths
           if (= 2 length) sum 1
           else sum take))

(iter-defun biome-query--unique-key-cands (name &optional max-words max-weight)
  "Generate unique key candidates for NAME.

The algorithm is as follows: NAME is split into words, each word
produces a list of all its prefixes.  E.g. \"hello\" produces \"\",
\"h\", \"he\", \"hel\", etc.  Numbers are taken as a whole,
e.g. \"100\" produces just \"\" and \"100\".

One key canditade is a concatenation of prefixes of the first
MAX-WORDS words, in the same order in which words appeared in NAME.

All possible key candidates are weighted by
`biome-query--unique-key-weight'.  The iteration yields these
candidates in ascending order of those weights, up to MAX-WEIGHT.

This algorithm has exponential computational complexity because
it sorts the cartesian product of all prefixes of each word, and it
gets pretty slow at more than 3 words.  Hence the words are truncated
at 3."
  (let ((name-low (replace-regexp-in-string (rx (not alnum))  " " (downcase name)))
        (generated-keys (make-hash-table :test 'equal)))
    (unless max-weight (setq max-weight 6))
    (unless max-words (setq max-words 3))
    (cl-loop for (key . value) in biome-query--split-items
             do (setq name-low
                      (replace-regexp-in-string (regexp-quote key) value name-low)))
    (let* ((items (cl-loop for item in (split-string name-low)
                           if (and (not (member item biome-query--ignore-items))
                                   (< (length res) max-words))
                           collect item into res
                           finally return res))
           (sequences (cl-loop for item in items
                               for is-num = (string-match-p (rx num) item)
                               if is-num
                               collect (number-sequence 0 (length item) (length item))
                               else if (not is-num)
                               collect (number-sequence 0 (length item) 1)))
           (seq-lengths (mapcar #'length sequences)))
      (dolist (item-take (thread-last
                           (reverse sequences)
                           (cl-reduce #'biome-query--cartesian-product)
                           (mapcar (lambda (it) (if (listp it) (nreverse it) (list it))))
                           ;; XXX this seems to be just a bit faster than `seq-sort-by'.
                           (mapcar (lambda (it)
                                     (cons (biome-query--unique-key-weight it seq-lengths) it)))
                           (seq-filter (lambda (it) (< (car it) max-weight)))
                           (funcall
                            (lambda (sequences) (sort sequences (lambda (a b) (< (car a) (car b))))))
                           (mapcar #'cdr)
                           (seq-sort-by
                            (lambda (it)
                              (biome-query--unique-key-weight it seq-lengths))
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

(defun biome-query--unique-keys (names &optional exclude)
  "Get unique keys for NAMES.

NAMES is a list of strings.  EXCLUDE is a list of strings to
exclude from the result."
  (let ((keys-by-name (make-hash-table :test 'equal))
        (names-by-key (make-hash-table :test 'equal))
        (iters (make-hash-table :test 'equal)))
    (cl-loop for name in names
             do (puthash name (biome-query--unique-key-cands name) iters))
    (while-let ((names-to-update
                 (append
                  ;; Unset forbidden keys
                  (cl-loop for name in names
                           for key-1 = (gethash name keys-by-name)
                           if (or (null key-1)
                                  (and
                                   exclude key-1
                                   (seq-some
                                    (lambda (ex) (string-prefix-p ex key-1))
                                    exclude)))
                           collect name)
                  ;; Duplicate keys
                  (cl-loop for names being the hash-values of names-by-key
                           if (< 1 (length names))
                           collect (car (seq-sort-by #'length #'> names)))
                  ;; Duplicate subkeys
                  (cl-loop for key being the hash-key of names-by-key
                           append (cl-loop
                                   for i from 1 to (1- (length key))
                                   for subkey = (seq-take key i)
                                   for dupe-names = (gethash subkey names-by-key)
                                   when dupe-names append dupe-names)))))
      ;; XXX I don't have the slighest idea why, but renaming the
      ;; variable `key-1' into `key' makes the byte-compiler complain
      ;; about `key' being unused.  Hope that's not a Heisenbug.
      (cl-loop
       for name in names-to-update
       for old-key = (gethash name keys-by-name)
       for key-1 = (iter-next (gethash name iters))
       if old-key do (puthash old-key (remove name (gethash old-key names-by-key)) names-by-key)
       do (puthash key-1 (cons name (gethash key-1 names-by-key)) names-by-key)
       do (puthash name key-1 keys-by-name)))
    keys-by-name))

(defun biome--query-section-fields-define-infixes (fields keys param infix-name)
  "Define infixes for FIELDS.

I wish it were possible to avoid that... Maybe I just didn't find the
correct way.

PARAM is the value of `:param' of the section.  INFIX-NAME is the
prefix for infix names.  KEYS is a hash table mapping field names
to keys."
  (cl-loop
   for field in fields
   for field-api-key = (car field)
   for name = (alist-get :name (cdr field))
   for key = (gethash name keys)
   for type = (alist-get :type (cdr field))
   for infix-symbol = (intern (concat infix-name field-api-key))
   do (eval (pcase type
              ('checkbox
               `(transient-define-infix ,infix-symbol ()
                  :class 'biome-query--transient-switch-variable
                  :key ,key
                  :api-key ,field-api-key
                  :param ,param
                  :description ,name
                  :argument ,name))
              ('date
               `(transient-define-infix ,infix-symbol ()
                  :class 'biome-query--transient-date-variable
                  :key ,key
                  :api-key ,field-api-key
                  :description ,name
                  :prompt ,name))
              ('select
               `(transient-define-infix ,infix-symbol ()
                  :class 'biome-query--transient-select-variable
                  :key ,key
                  :api-key ,field-api-key
                  :description ,name
                  :options ',(alist-get :options (cdr field))))
              ((or 'number 'integer 'float)
               `(transient-define-infix ,infix-symbol ()
                  :class 'biome-query--transient-number-variable
                  :key ,key
                  :api-key ,field-api-key
                  :description ,name
                  :integer ,(eq type 'integer)
                  :min ,(alist-get :min (cdr field))
                  :max ,(alist-get :max (cdr field))))
              ('timezone
               `(transient-define-infix ,infix-symbol ()
                  :class 'biome-query--transient-timezone-variable
                  :key ,key
                  :api-key ,field-api-key
                  :description ,name))
              (_
               `(transient-define-infix ,infix-symbol ()
                  :key ,key
                  :description ,name
                  :argument ,name))))))

(defun biome-query--section-fields-children (fields keys parents cache-key)
  "Get transient layout for FIELDS.

FIELDS is a list of fields as defined in `biome-api-parse--page'.
KEYS is the result of `biome-query--unique-keys'.  PARENTS is a list
of parent sections.  CACHE-KEY is the string that uniquely identifies
the position of the current section in the `biome-api-data' tree."
  (when fields
    (let ((param (seq-some (lambda (s) (alist-get :param s)) parents))
          (infix-name (concat "biome-query--transient-" cache-key "-")))
      (biome--query-section-fields-define-infixes fields keys param infix-name)

      `(["Fields"
         :class transient-columns
         ,@(thread-last
             (append
              fields
              (when (string-match-p
                     (rx "Select Coordinates")
                     (alist-get :name (car parents)))
                '(coords)))
             (seq-map-indexed
              (lambda (field idx) (cons field (/ idx biome-query-max-fields-in-row))))
             (seq-group-by #'cdr)
             (mapcar
              (lambda (group)
                (apply
                 #'vector
                 (mapcar
                  (lambda (el)
                    (pcase (car el)
                      ('coords
                       '(biome-query--transient-coords-infix))
                      (_ (let* ((field-api-key (caar el)))
                           (list (intern (concat infix-name field-api-key)))))))
                  (cdr group))))))]))))


(defun biome-query--section-sections-children (sections keys parents)
  "Get transient layout for SECTIONS.

SECTIONS is a list of sections as defined in `biome-api-parse--page'.
KEYS is the result of `biome-query--unique-keys'.  PARENTS is a
list of parent sections."
  (when sections
    `(["Sections"
       :class ,(if (length> sections biome-query--max-sections-for-row)
                   'transient-column
                 'transient-row)
       ,@(mapcar
          (lambda (section)
            `(,(gethash (alist-get :name section) keys)
              ,(alist-get :name section)
              (lambda ()
                (interactive)
                (let* ((section ',section)
                       (param (alist-get :param section)))
                  (if (or (null param)
                          (equal param (alist-get :group biome-query-current))
                          (not (member param biome-query-groups)))
                      (biome-query--section section ',parents)
                    (message "Need to activate group: %s" param))))
              :transient transient--do-stack))
          sections)])))

(defmacro biome-query--with-layout-cache (cache-key &rest body)
  "Cache layout for CACHE-KEY in `biome-query--layout-cache'.

BODY is the body of the macro that returns the layout.  The resulting
expression either returns the cached layout or evaluates BODY, caches
the result and returns it.

This is necessary mostly because `biome-query--unique-keys' is
sometimes a bit slow."
  (declare (indent 1))
  `(let ((layout (gethash ,cache-key biome-query--layout-cache)))
     (if layout
         layout
       (let* ((cache-key ,cache-key)
              (layout (progn ,@body)))
         (puthash ,cache-key layout biome-query--layout-cache)
         layout))))

(defun biome-query--section-layout (section parents)
  "Get transient layout for SECTION.

SECTION is a form as defined by `biome-api-parse--page'.  PARENTS
is a list of parent sections."
  (biome-query--with-layout-cache
      (string-join
       (mapcar
        (lambda (s) (compat-call string-replace " " "-" s))
        (cons
         (alist-get :name section)
         (mapcar (lambda (s) (alist-get :name s)) parents)))
       "-")
    (let* ((sections (or (alist-get :children section)
                         (alist-get :sections section)))
           (fields (alist-get :fields section))
           (keys (biome-query--unique-keys
                  (append
                   (mapcar (lambda (s) (alist-get :name s)) sections)
                   (mapcar (lambda (s) (alist-get :name (cdr s))) fields))
                  '("q")))
           (parents (cons section parents)))
      (append
       (biome-query--section-fields-children fields keys parents cache-key)
       (biome-query--section-sections-children sections keys parents)))))

(defun biome-query--transient-prepare-layout (name suffixes)
  "Prepare dynamic transient layout for NAME.

SUFFIXES is a list of suffix definitions."
  (thread-last
    suffixes
    (cl-mapcan (lambda (s) (transient--parse-child 'tsc-dynamic-layout s)))
    (mapcar #'eval)
    (put name 'transient--layout)))

(defun biome-query--section-groups (section)
  "Get list of groups for SECTION, e.g. hourly, daily, etc."
  (cl-loop for child in (alist-get :sections section)
           for group = (alist-get :param child)
           when (and group (member group biome-query-groups))
           collect (cons group (alist-get :name child))))

(defun biome-query--reset-report ()
  "Reset the current query to the current section."
  (interactive)
  (setq biome-query-current
        (copy-tree
         `((:name . ,(alist-get :name biome-query--current-section))
           (:group . ,(caar (biome-query--section-groups biome-query--current-section)))
           (:params . nil)))))

(defun biome-query--on-return ()
  "Process the query made by `biome-query'."
  (interactive)
  (unless biome-query--callback
    (error "Biome-query--callback is not set"))
  (funcall biome-query--callback biome-query-current))

(defun biome-query--generate-preset ()
  "Generate a preset for the current query."
  (interactive)
  (let ((buf (generate-new-buffer "*biome-preset*"))
        (preset-symbol (gensym "biome-query-preset-")))
    (with-current-buffer buf
      (emacs-lisp-mode)
      (insert ";; Add this to your config\n")
      (insert (pp-to-string `(biome-def-preset ,preset-symbol
                               ,biome-query-current)))
      (insert ";; invoke with M-x " (symbol-name preset-symbol))
      (insert "\n\n;; Or:\n")
      (insert (pp-to-string `(add-to-list 'biome-presets-alist
                                          '(,(symbol-name preset-symbol)
                                            :normal
                                            ,biome-query-current))))
      (insert ";; invoke with M-x biome-preset"))
    (switch-to-buffer buf)))

(transient-define-prefix biome-query--section (section &optional parents)
  "Render transient for SECTION.

SECTION is a form as defined in `biome-api-parse--page'."
  (interactive (list nil))
  (unwind-protect
      (progn
        (biome-query--transient-prepare-layout
         'biome-query--section
         (append
          '([(biome-query--transient-path-infix)])
          '([(biome-query--transient-report-infix)])
          (unless parents
            '([(biome-query--transient-group-switch-infix)]))
          (biome-query--section-layout section parents)
          `(["Actions"
             :class transient-row
             ("RET" "Run" biome-query--on-return)
             ("q" "Up" transient-quit-one)
             ("Q" "Quit" transient-quit-all)
             ,@(unless parents
                 '(("P" "Generate preset definition" biome-query--generate-preset)
                   ("R" "Reset" biome-query--reset-report :transient t)))])))
        (transient-setup 'biome-query--section nil nil :scope
                         `((:section . ,section)
                           (:parents . ,parents))))
    (put 'biome-query-section 'transient--layout nil)))

(defun biome-query--section-open-params (params)
  "Open section defined by PARAMS in `biome-query--section'.

PARAMS is a form as defiend by `biome-api-data'."
  (setq biome-query--current-section params)
  (when (and biome-query-current
             (not (equal (alist-get :name params)
                         (alist-get :name biome-query-current))))
    (setq biome-query-current nil))
  (unless biome-query-current
    (biome-query--reset-report))
  (funcall-interactively #'biome-query--section params))

(defun biome-query--section-open (name)
  "Open section NAME in `biome-query--section'."
  (let ((params (alist-get name biome-api-data nil nil #'equal)))
    (cond
     ((equal name "Historical Weather (on this day)")
      (biome-multi-history))
     (params (biome-query--section-open-params params))
     (t (error "No such section: %s" name)))))

(transient-define-prefix biome-query (callback)
  ["Open Meteo Data"
   :class transient-column
   :setup-children
   (lambda (_)
     (transient-parse-suffixes
      'transient--prefix
      (cl-loop for (name . params) in biome-api-data
               collect `(,(alist-get :key params)
                         ,name
                         (lambda () (interactive)
                           (biome-query--section-open ,name))
                         :transient transient--do-stack))))]
  ["Aggregate Data"
   :class transient-column
   ("i" "Historical Weather (on this day)" biome-multi-history
    :transient transient--do-stack)
   ("u" "Join multiple queries" biome-multi :transient transient--do-stack)]
  ["Actions"
   :class transient-row
   ("r" "Resume" biome-resume :transient transient--do-stack)
   ("p" "Preset" biome-preset :transient transient--do-stack)
   ("q" "Quit" transient-quit-one)]
  (interactive (list nil))
  (unless callback
    (error "Callback is not set.  Run M-x `biome' instead"))
  (setq biome-query--callback callback)
  (transient-setup 'biome-query))

(provide 'biome-query)
;;; biome-query.el ends here
