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
(require 'font-lock)
(require 'org)
(require 'compat)
(require 'transient)

(defcustom biome-query-max-fields-in-row 20
  "Maximum number of fields in a row."
  :type 'integer
  :group 'biome)

(defcustom biome-query-completing-read-threshold 6
  "Invoke `completing-read' when there are more than this many choices."
  :type 'integer
  :group 'biome)

(defconst biome-query-groups '("daily" "hourly" "minutely_15" "hourly")
  "Name of groups.

A group is a mutually exclusive choice.  E.g. in the \"Weather
Forecast\" report you can choose between \"daily\" and \"hourly\".  In
principle, the API can return results for both groups, but they would
have to be displayed separately.")

(defconst biome-query--split-items '(("timezone" . "time zone")
                                     ("timeformat" . "time format")
                                     ("weathercode" . "weather code"))
  "Items to split into separate words for generating keys.")

(defconst biome-query--ignore-items '("m" "cm")
  "Items to ignore when generating unique keys.")

(defvar biome-query-current nil
  "Current report.

It is an alist with the following keys:
- `:name' - name of the root section.
- `:group' - name of the group (see `biome-query-groups').
- `:params' - alist with parameters, where the key is either nil (for
  global parameters) or the value of `:param' key of the corresponding
  section.

In the former case, the value is an alist with values; in the latter
case, the value is a list of variable names available in the group.")

(defvar biome-query--current-section nil
  "Current section.")

(defvar biome-query--layout-cache (make-hash-table :test 'equal)
  "Cache for dynamic transient layout.")

;; TODO delete this
(setq biome-query--layout-cache (make-hash-table :test 'equal))

;; Transient display classes
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

(defclass biome-query--transient-path (transient-suffix)
  ((transient :initform t))
  "A transient class to display the current path.")

(cl-defmethod transient-init-value ((_ biome-query--transient-path))
  "A dummy method for `biome-query--transient-report'."
  nil)

(cl-defmethod transient-format ((_ biome-query--transient-path))
  "Format the current path."
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
        (not
         (null
          (member
           (oref obj api-key)
           (if-let ((param (oref obj param)))
               (cdr
                (assoc
                 param
                 (alist-get :params biome-query-current)))
             (alist-get :params biome-query-current)))))))

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
  "Return a string generated using OBJ's `format'.
%k is formatted using `transient-format-key'.
%d is formatted using `transient-format-description'.
%v is formatted using `transient-format-value'."
  (concat
   (string-pad (transient-format-key obj) 6)
   (transient-format-description obj)
   (when (oref obj value)
     (propertize " (+)" 'face 'transient-argument))))

(defclass biome-query--transient-variable (transient-variable)
  ((api-key :initarg :api-key))
  "A transient class to display a variable.")

(cl-defmethod transient-infix-set ((obj biome-query--transient-variable) value)
  "Set the value of OBJ to VALUE.

OBJ is an instance of `biome-query--transient-date-variable'."
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

(defclass biome-query--transient-date-variable (biome-query--transient-variable)
  ((reader :initform #'biome-query--transient-date-reader))
  "A transient class to display a date variable.")

(defun biome-query--transient-date-reader (prompt _initial-input _history)
  "Read the date with `org-read-date'.

PROMPT is a string to prompt with.

Returns a UNIX timestamp."
  (time-convert
   (org-read-date nil t nil prompt)
   'integer))

(cl-defmethod transient-format-value ((obj biome-query--transient-date-variable))
  "Format the value of OBJ.

OBJ is an instance of `biome-query--transient-date-variable'."
  (let ((value (if (slot-boundp obj 'value) (slot-value obj 'value) nil)))
    (if value
        (propertize
         (format-time-string
          ;; TODO fix
          org-journal-date-format
          (seconds-to-time
           value))
         'face 'transient-value)
      (propertize "unset" 'face 'transient-inactive-value))))

(defclass biome-query--transient-select-variable (biome-query--transient-variable)
  ((options :initarg :options)))

(cl-defmethod transient-infix-value ((obj biome-query--transient-select-variable))
  (oref obj value))

(cl-defmethod transient-format-value ((obj biome-query--transient-select-variable))
  "Format the value of OBJ."
  (let ((value (transient-infix-value obj)))
    (concat
     (propertize "[" 'face 'transient-inactive-value)
     (mapconcat
      (lambda (choice)
        (propertize (cdr choice) 'face
                    (if (eq (car choice) value)
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
  "A transient class to display a number variable.")

(cl-defmethod transient-infix-read ((obj biome-query--transient-variable))
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
  (let ((value (if (slot-boundp obj 'value) (slot-value obj 'value) nil)))
    (if value
        (propertize
         (number-to-string value)
         'face 'transient-value)
      (propertize "unset" 'face 'transient-inactive-value))))

(defclass biome-query--transient-timezone-variable (biome-query--transient-variable) ()
  "A transient class to display a timezone variable.")

(cl-defmethod transient-infix-read ((obj biome-query--transient-timezone-variable))
  "Read the value of OBJ."
  (completing-read (concat (oref obj description) " ") biome-api-timezones
                   nil t (oref obj value)))

(defclass biome-query--transient-group-switch (biome-query--transient-select-variable)
  ((options :initform nil))
  "A transient class to switch between groups of a query.")

(cl-defmethod transient-infix-value ((obj biome-query--transient-group-switch))
  (oref obj value))

(cl-defmethod transient-init-value ((obj biome-query--transient-group-switch))
  (let ((groups (biome-query--section-groups biome-query--current-section)))
    (oset obj options groups)
    (oset obj value (alist-get :group biome-query-current))))

(cl-defmethod transient-infix-read ((obj biome-query--transient-group-switch))
  "Read the value of OBJ."
  (setq my/test (list (oref obj options) (oref obj value)))
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
    (setf (oref obj value) value)
    (transient-update)))

(transient-define-infix biome-query--transient-group-switch-infix ()
  :class 'biome-query--transient-group-switch
  :key "S"
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
  ;; TODO better weight function
  (cl-loop for take in it
           for length in seq-lengths
           if (= 2 length) sum 1
           else sum take))

(iter-defun biome-query--unique-key-cands (name &optional max-words max-weight)
  "Generate unique key candidates for NAME.

The algorithm is as follows: NAME is split into words, each word
produces a list of all its prefixes. E.g. \"hello\" produces \"\",
\"h\", \"he\", \"hel\", etc.  Numbers are takes as a whole,
e.g. \"100\" produces just \"\" and \"100\".

One key canditate is a concatenation of prefixes of the first
MAX-WORDS words, in the same order in which words appeared in NAME.

All possible key candidates are weighted by
`biome-query--unique-key-weight'.  The iteration yields these
candidates in the ascending order by these weights, up to MAX-WEIGHT.

This algorithm has exponential computational complexity because
it sorts the cartesian product of all prefixes of each word, and it
gets pretty slow at more than 3 words.  Hence the words are truncated
at 3."
  (let ((name-low (replace-regexp-in-string (rx (not alnum))  " " (downcase name)))
        (generated-keys (make-hash-table :test 'equal))
        (max-weight (or max-weight 6))
        (max-words (or max-words 3)))
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
                           (reduce #'biome-query--cartesian-product)
                           (mapcar (lambda (it) (if (listp it) (nreverse it) (list it))))
                           ;; TODO delete comment
                           ;; ((lambda (kek) (message "Sorting %s" (length kek)) kek))
                           ;; XXX this seems to be just a bit faster than `seq-sort-by'.
                           (mapcar (lambda (it)
                                     (cons (biome-query--unique-key-weight it seq-lengths) it)))
                           (seq-filter (lambda (it) (< (car it) max-weight)))
                           ((lambda (sequences) (sort sequences (lambda (a b) (< (car a) (car b))))))
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
                           for key = (gethash name keys-by-name)
                           if (or (null key) (and
                                              exclude key
                                              (seq-some
                                               (lambda (ex) (string-prefix-p ex key))
                                               exclude)))
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

(defun biome--query-section-fields-define-infixes (fields keys param infix-name)
  "Define infixes for FIELDS.

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
  "Get transient laoyut for FIELDS.

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
             fields
             (seq-map-indexed
              (lambda (field idx) (cons field (/ idx biome-query-max-fields-in-row))))
             (seq-group-by #'cdr)
             (mapcar
              (lambda (group)
                (apply
                 #'vector
                 (mapcar
                  (lambda (el)
                    (let* ((field-api-key (caar el)))
                      (list (intern (concat infix-name field-api-key)))))
                  (cdr group))))))]))))

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

(defmacro biome-query--with-layout-cache (cache-key &rest body)
  "Cache layout for CACHE-KEY.

BODY is the body of the macro."
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
        (lambda (s) (string-replace " " "-" s))
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
  (interactive)
  (setq biome-query-current
        (copy-tree
         `((:name . ,(alist-get :name biome-query--current-section))
           (:group . ,(caar (biome-query--section-groups biome-query--current-section)))
           (:params . nil)))))

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
             ("q" "Up" transient-quit-one)
             ("Q" "Quit" transient-quit-all)
             ,(unless parents
                '("R" "Reset" biome-query--reset-report :transient t))])))
        (transient-setup 'biome-query--section nil nil :scope
                         `((:section . ,section)
                           (:parents . ,parents))))
    (put 'biome-query-section 'transient--layout nil)))

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
                           (setq biome-query--current-section ',params)
                           (when (and biome-query-current
                                      (not (equal ,name (alist-get :name biome-query-current))))
                             (setq biome-query-current nil))
                           (unless biome-query-current
                             (biome-query--reset-report))
                           (biome-query--section ',params))
                         :transient transient--do-replace))))]
  ["Actions"
   ("q" "Quit" transient-quit-one)])

(provide 'biome-query)
;;; biome-query.el ends here
